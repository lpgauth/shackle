-module(shackle).
-include("shackle_internal.hrl").
-include("shackle_pool.hrl").

-compile(inline).
-compile({inline_size, 512}).

%% public
-export([
    call/2,
    call/3,
    call/4,
    cast/2,
    cast/3,
    cast/4,
    receive_response/1,
    receive_response/2,
    set_probe_callback/2,
    set_socket_rtt_callback/2,
    start_probe/3,
    stop_probe/1
]).

%% types
-type cast() :: #cast {}.
-type client() :: module().
-type external_request_id() :: term().
-type inet_address() :: inet:ip_address() | inet:hostname().
-type inet_port() :: inet:port_number().
-type protocol() :: shackle_ssl| shackle_tcp | shackle_udp.
-type request_id() :: {shackle_server:name(), reference()}.
-type request_ref() :: reference().
-type reply() :: term().
-type response() :: {external_request_id(), term()}.
-type socket() :: inet:socket() | ssl:sslsocket().
-type socket_option() :: gen_tcp:connect_option() | gen_udp:option() | ssl:tls_client_option().
-type socket_options() :: [socket_option()].
-type table() :: atom().
-type timeout_ms() :: pos_integer().
-type timeout_x() :: infinity | timeout_ms().
-type time() :: pos_integer().
-type metric_type() :: counter | timing.
-type metric_key() :: iodata().
-type metric_value() :: integer().

-export_type([
    cast/0,
    client/0,
    external_request_id/0,
    inet_address/0,
    inet_port/0,
    metric_type/0,
    metric_key/0,
    metric_value/0,
    protocol/0,
    request_id/0,
    request_ref/0,
    reply/0,
    response/0,
    socket/0,
    socket_options/0,
    table/0,
    time/0
]).

%% public

-doc """
Processes a request. Parameters:

- `PoolName` - The name of connection pool
- `Request` - The request to process

Returns:

A reply term.
""".
-spec call(shackle_pool:name(), term()) ->
    term() | {error, term()}.
call(PoolName, Request) ->
    call(PoolName, Request, ?DEFAULT_TIMEOUT, infinity).

-doc """
Processes a request. Parameters:

- `PoolName` - The name of connection pool
- `Request` - The request to process
- `Timeout` - The time period allocated to process the requests

Returns:

A reply term
""".
-spec call(atom(), term(), timeout_x()) ->
    term() | {error, atom()}.
call(PoolName, Request, Timeout) ->
    call(PoolName, Request, Timeout, Timeout).

%% @private
%% Reserved for testing
-spec call(atom(), term(), timeout_x(), timeout_x()) ->
    term() | {error, atom()}.
call(PoolName, Request, Timeout, RecvTimeout) ->
    case shackle_pool:options(PoolName) of
        {ok, #pool_options{client = Client} = PoolOpts} ->
            Fun = call_impl_fun(PoolOpts, Request, Timeout, RecvTimeout),
            Dispatcher = shackle_observe:dispatcher(),
            Dispatcher:call(PoolName, Client, Fun);
        {error, Reason} ->
            {error, Reason}
    end.

call_impl_fun(PoolOpts, Request, Timeout, RecvTimeout) ->
    fun() ->
        case cast_impl(PoolOpts, Request, self(), Timeout, os:system_time(microsecond)) of
            {ok, RequestId} ->
                try
                    receive_response(RequestId, RecvTimeout)
                catch error:timeout ->
                    {error, timeout}
                end;
            {error, Reason} ->
                {error, Reason}
        end
    end.

-doc """
Processes a request. Parameters:

- `PoolName` - The name of connection pool
- `Request` - The request to process

Returns:

The request ID of the request being processed
""".
-spec cast(shackle_pool:name(), term()) ->
    {ok, request_id()} | {error, atom()}.
cast(PoolName, Request) ->
    cast(PoolName, Request, self()).

-doc """
Processes a request. Parameters:

- `PoolName` - The name of connection pool
- `Request` - The request to process
- `Pid` - The process identifier to process the requests

Returns:

The request ID of the request being processed or error:
`{ok, RequestID} | {error, Reason}`
""".
-spec cast(shackle_pool:name(), term(), undefined | pid()) ->
    {ok, request_id()} | {error, atom()}.
cast(PoolName, Request, Pid) when is_pid(Pid); Pid == undefined ->
    cast(PoolName, Request, Pid, ?DEFAULT_TIMEOUT).

-doc """
Processes a request. Parameters:

- `PoolName` - The name of connection pool
- `Request`  - The request to process
- `Pid`      - The process identifier to process the requests
- `Timeout`  - The time period allocated to process the requests

Returns:

The request ID of the request being processed or error:
`{ok, RequestID} | {error, Reason}`
""".
-spec cast(shackle_pool:name(), term(), undefined | pid(), timeout_x()) ->
    {ok, request_id()} | {error, atom()}.
cast(PoolName, Request, Pid, Timeout) ->
    case shackle_pool:options(PoolName) of
        {ok, #pool_options{client = Client} = PoolOpts} ->
            Dispatcher = shackle_observe:dispatcher(),
            Dispatcher:cast(PoolName, Client, fun() ->
                cast_impl(PoolOpts, Request, Pid, Timeout, os:system_time(microsecond))
            end);
        {error, Reason} ->
            {error, Reason}
    end.

-spec cast_impl(shackle_pool:pool_options(), term(), undefined | pid(), timeout_x(), non_neg_integer()) ->
    {ok, request_id()} | {error, atom()}.
cast_impl(PoolOpts, Request, Pid, Timeout, Timestamp) ->
    case shackle_pool:server(PoolOpts) of
        {ok, Client, Server, Sema} ->
            Cast = mk_cast(Client, Server, Pid, Sema, Timestamp, Timeout),
            try
                Server ! {request, [Cast, Request]},
                {ok, Cast#cast.request_id}
            catch error:badarg ->
                {error, {bad_server, Server}}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

-doc """
Receive response for the list of requests. Parameters:

- `RequestId` - The request ID of the request being processed

Returns:

A reply: `{ok, Reply} | {error, Reason}`
""".
-spec receive_response(request_id()) ->
    [{ok, reply()} | {error, no_reply | timeout}] | {error, term()}.
receive_response(RequestId) ->
    receive_response(RequestId, infinity).

-doc """
Receive response for the list of requests.

Parameters:

- `RequestId` - The request ID of the request being processed

Returns:

A reply.

The function may throw a timeout exception if there is no response received and
a timeout is reached.
""".
-spec receive_response(request_id(), non_neg_integer()|infinity) ->
    reply() | {error, term()}.
receive_response(RequestId, Timeout) ->
    receive
        {#cast {request_id = ID}, Reply} when ID == RequestId ->
            Reply
    after Timeout ->
        %% Since the legacy signature of this function returns the `Reply', we
        %% have no choice but to throw an exception here to make it
        %% distinguishable from the normal reply.
        erlang:error(timeout)
    end.

-doc """
Register a socket RTT callback for a specific shackle client.

The callback `Fun(PoolName, RttUs)` is invoked on each TCP data receive with the
kernel-reported socket RTT in microseconds (Linux TCP_INFO). Silently no-ops on
non-Linux or non-TCP sockets.
""".
-spec set_socket_rtt_callback(client(), fun((shackle_pool:name(), non_neg_integer()) -> any())) -> ok.
set_socket_rtt_callback(Client, Fun) when is_atom(Client), is_function(Fun, 2) ->
    persistent_term:put({shackle_socket_rtt_callback, Client}, Fun),
    ok.

-spec set_probe_callback(client(), fun((shackle_pool:name(), integer()) -> any())) -> ok.
set_probe_callback(Client, Fun) when is_atom(Client), is_function(Fun, 2) ->
    persistent_term:put({shackle_probe_callback, Client}, Fun),
    ok.

-spec start_probe(shackle_pool:name(), client(), map()) -> {ok, pid()} | {error, term()}.
start_probe(PoolName, Client, Opts) ->
    ChildSpec = #{
        id => {shackle_probe, PoolName},
        start => {shackle_probe, start_link, [PoolName, Client, Opts]},
        restart => permanent,
        type => worker
    },
    supervisor:start_child(?SUPERVISOR, ChildSpec).

-spec stop_probe(shackle_pool:name()) -> ok | {error, term()}.
stop_probe(PoolName) ->
    Id = {shackle_probe, PoolName},
    case supervisor:terminate_child(?SUPERVISOR, Id) of
        ok -> supervisor:delete_child(?SUPERVISOR, Id);
        Err -> Err
    end.

%% private

mk_cast(Client, Server, Pid0, Sema, Timestamp, Timeout) ->
    {Pid, SendReply} =
        if is_pid(Pid0) ->
            {Pid0, true};
        true ->
            {self(), false}
        end,
    #cast {
        client = Client,
        pid = Pid,
        send_reply = SendReply,
        request_id = {Server, make_ref()},
        sema = Sema,    % Semaphore reference to release on cast reply
        timeout = Timeout,
        timestamp = Timestamp
    }.
