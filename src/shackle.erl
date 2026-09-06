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

A reply term or error.
""".
-spec call(shackle_pool:name(), term()) ->
    term() | {error, term()}.
call(PoolName, Request) ->
    call(PoolName, Request, #{}).

-doc """
Processes a request. Parameters can be:
- Timeout (integer/infinity): `call(Pool, Request, Timeout)` - old API
- Options (map): `call(Pool, Request, #{timeout => 1000})` - new API

Returns:

A reply term or error.

Examples:
```erlang
% Old API with timeout
shackle:call(my_pool, {get, "key"}, 1000).

% New API with options map
shackle:call(my_pool, {get, "key"}, #{timeout => 1000}).

% With hash_key for consistent_hash strategy
shackle:call(my_pool, {get, "key"}, #{hash_key => "key"}).

% With both timeout and hash_key
shackle:call(my_pool, {get, "key"}, #{timeout => 1000, hash_key => "key"}).
```
""".
-spec call(atom(), term(), timeout_x() | map()) ->
    term() | {error, atom()}.
call(PoolName, Request, Options) when is_map(Options) ->
    case shackle_pool:options(PoolName) of
        {ok, #pool_options{client = Client} = PoolOpts} ->
            Fun = fun() -> call_impl(PoolOpts, Request, Options) end,
            Dispatcher = shackle_observe:dispatcher(),
            Dispatcher:call(PoolName, Client, Fun);
        {error, Reason} ->
            {error, Reason}
    end;
call(PoolName, Request, Timeout) when is_integer(Timeout) orelse Timeout == infinity ->
    %% Old API - convert timeout to options map
    call(PoolName, Request, #{timeout => Timeout}).

%% @private
%% Reserved for testing - old API with explicit timeouts
-spec call(atom(), term(), timeout_x(), timeout_x()) ->
    term() | {error, atom()}.
call(PoolName, Request, Timeout, RecvTimeout) ->
    call(PoolName, Request, #{timeout => Timeout, recv_timeout => RecvTimeout}).

call_impl(PoolOpts, Request, Options) ->
    Now = now_time(),
    case cast_impl(PoolOpts, Request, self(), Now, Options) of
        {ok, RequestId} ->
            try
                RecvTimeout = get_recv_timeout(Options),
                receive_response(RequestId, RecvTimeout)
            catch error:timeout ->
                {error, timeout}
            end;
        {error, Reason} ->
            {error, Reason}
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
Processes a request. Third parameter can be:
- Pid (process to send response to): `cast(Pool, Request, Pid)`
- Options (map): `cast(Pool, Request, #{hash_key => Key, timeout => Timeout})`

When Pid is provided, uses default timeout.
When Options are provided, sends to self() with timeout from map.

Options map keys:
- `timeout` (default: infinity) - Time period to process the request
- `hash_key` - Key for consistent_hash strategy

Examples:
```erlang
% Send response to specific process
shackle:cast(my_pool, {get, "key"}, Pid).

% With options for consistent_hash strategy, response to self()
shackle:cast(my_pool, {get, "key"}, #{hash_key => "key"}).

% With timeout in options
shackle:cast(my_pool, {get, "key"}, #{timeout => 1000, hash_key => "key"}).
```
""".
-spec cast(shackle_pool:name(), term(), undefined | pid() | map()) ->
    {ok, request_id()} | {error, atom()}.
cast(PoolName, Request, Options) when is_map(Options) ->
    cast(PoolName, Request, self(), Options);
cast(PoolName, Request, Pid) when is_pid(Pid); Pid == undefined ->
    cast(PoolName, Request, Pid, #{}).

-doc """
Processes a request with timeout or options. Parameters:

- `PoolName` - The name of connection pool
- `Request`  - The request to process
- `Pid`      - The process identifier to process the requests
- `TimeoutOrOptions` - Either:
  - Timeout (for backward compatibility)
  - Options map (new API with hash_key, timeout, etc.)

Returns:

The request ID of the request being processed or error:
`{ok, RequestID} | {error, Reason}`

Examples:
```erlang
% Old API with explicit timeout
shackle:cast(my_pool, {get, "key"}, self(), 1000).

% New API with options
shackle:cast(my_pool, {get, "key"}, self(), #{timeout => 1000, hash_key => "key"}).
```
""".
-spec cast(shackle_pool:name(), term(), undefined | pid(), timeout_x() | map()) ->
    {ok, request_id()} | {error, atom()}.
cast(PoolName, Request, Pid, Options) when is_map(Options) ->
    Timestamp = now_time(),
    case shackle_pool:options(PoolName) of
        {ok, #pool_options{client = Client} = PoolOpts} ->
            Dispatcher = shackle_observe:dispatcher(),
            Dispatcher:cast(PoolName, Client, fun() ->
                cast_impl(PoolOpts, Request, Pid, Timestamp, Options)
            end);
        {error, Reason} ->
            {error, Reason}
    end;
cast(PoolName, Request, Pid, Timeout) when is_integer(Timeout) orelse Timeout == infinity ->
    %% Old API - convert timeout to options map
    cast(PoolName, Request, Pid, #{timeout => Timeout}).

-spec cast_impl(shackle_pool:pool_options(), term(), undefined | pid(), non_neg_integer(), map()) ->
    {ok, request_id()} | {error, atom()}.
cast_impl(PoolOpts, Request, Pid, Timestamp, Options) ->
    case shackle_pool:server(PoolOpts, Options) of
        {ok, Client, Server, Sema} ->
            Cast = mk_cast(Client, Server, Pid, Sema, Timestamp, Options),
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

mk_cast(Client, Server, Pid0, Sema, Timestamp, Options) ->
    {Pid, SendReply} =
        if is_pid(Pid0) ->
            {Pid0, true};
        true ->
            {self(), false}
        end,
    #cast {
        client     = Client,
        pid        = Pid,
        send_reply = SendReply,
        request_id = {Server, make_ref()},
        sema       = Sema,    % Semaphore reference to release on cast reply
        timeout    = maps:get(timeout, Options, ?DEFAULT_TIMEOUT),
        timestamp  = Timestamp
    }.

-spec now_time() -> time().
now_time() ->
    os:system_time(microsecond).

get_recv_timeout(Options) ->
  case maps:get(recv_timeout, Options, undefined) of
    undefined ->
      maps:get(timeout, Options, infinity);
    Timeout ->
      Timeout
  end.
