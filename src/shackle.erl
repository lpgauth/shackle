-module(shackle).
-include("shackle_internal.hrl").
-include("shackle_pool.hrl").

-compile(inline).
-compile({inline_size, 512}).

%% public
-export([
    batch_call/2,
    batch_call/3,
    batch_call/4,
    batch_call_expect_ordered_replies/2,
    batch_call_expect_ordered_replies/3,
    batch_cast/2,
    batch_cast/3,
    batch_cast/4,
    call/2,
    call/3,
    call/4,
    cast/2,
    cast/3,
    cast/4,
    multi_call/3,
    receive_batch_expect_ordered_replies/1,
    receive_batch_response/1,
    receive_batch_response/2,
    receive_response/1,
    receive_response/2,
    set_probe_callback/2,
    set_socket_rtt_callback/2,
    start_probe/3,
    stop_probe/1
]).

%% types
-type batch_ref() :: undefined | reference().
-type batch_state() :: {batch_ref(), pos_integer(), [{request_ref(), term()}]}.
-type cast() :: #cast {}.
-type client() :: module().
-type external_request_id() :: term().
-type inet_address() :: inet:ip_address() | inet:hostname().
-type inet_port() :: inet:port_number().
-type protocol() :: shackle_ssl| shackle_tcp | shackle_udp.
-type request() :: term().
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
    batch_ref/0,
    batch_state/0,
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
            Fun = cast_impl_fun(PoolOpts, Request, Timeout, RecvTimeout),
            Dispatcher = shackle_observe:dispatcher(),
            Dispatcher:call(PoolName, Client, Fun);
        {error, Reason} ->
            {error, Reason}
    end.

cast_impl_fun(PoolOpts, Request, Timeout, RecvTimeout) ->
    fun() ->
        Now = now_time(),
        case cast_impl(PoolOpts, Request, self(), Timeout, Now) of
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
                cast_impl(PoolOpts, Request, Pid, Timeout, now_time())
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
Processes a list of requests. Parameters:

- `PoolName` - The name of connection pool
- `Requests` - The list of requests

Returns:

A list of replies for each request:
`[{ok, RequestID} | {error, Reason}]`

""".
-spec batch_call(shackle_pool:name(), [term()]) ->
    [{ok, reply()} | {error, no_reply | timeout}] | {error, term()}.
batch_call(PoolName, Requests) ->
    batch_call(PoolName, Requests, ?DEFAULT_TIMEOUT).

-doc """
Processes a list of requests. Parameters:

- `PoolName` - The name of connection pool
- `Requests` - The list of requests
- `Timeout` - The time period allocated to process the requests

Returns:

A list of replies for each request:
`[{ok, RequestID} | {error, Reason}]`
""".
-spec batch_call(atom(), [term()], timeout_x()) ->
    [{ok, reply()} | {error, no_reply | timeout}] | {error, term()}.
batch_call(PoolName, Requests, Timeout) ->
    batch_call(PoolName, Requests, Timeout, Timeout).

%% @private
%% Reserved for testing
-spec batch_call(atom(), [term()], timeout_x(), timeout_x()) ->
    [{ok, reply()} | {error, no_reply | timeout}] | {error, term()}.
batch_call(_, [], _, _) ->
    [];
batch_call(PoolName, Requests, Timeout, RecvTimeout) when is_list(Requests) ->
    case batch_cast(PoolName, Requests, self(), Timeout) of
        {ok, {_BatchRef, _Count, RequestRefs} = BatchState} ->
            Replies = maps:from_list(receive_batch_response(BatchState, [], RecvTimeout)),
            [maps:get(ReqRef, Replies, {error, no_reply}) || {ReqRef, _} <- RequestRefs];
        {error, Reason} ->
            {error, Reason}
    end.

-doc """
Processes a list of requests.

Assumes that replies arrive in the order of submitted requests. A batch of
requests may return the same number (or fewer) replies.  If the underlying
protocol is such that a server answers with a subset of replies for a batch of
requests, the requests with `RequestIDs` that don't have replies will contain
`{error, no_reply}`.  When some requests timeout while waiting for the server's
reply, they would return `{error, timeout}`.

When there is a problem calling the server, the function returns `{error,
Reason}`.

Parameters:

- `PoolName` - The name of connection pool
- `Requests` - The list of requests

Returns:

A list of replies for each request:
`[{ok, RequestID} | {error, Reason}]`
""".
-spec batch_call_expect_ordered_replies(atom(), [request()]) ->
    [{ok, reply()} | {error, no_reply | timeout}] | {error, term()}.
batch_call_expect_ordered_replies(_, []) ->
    [];
batch_call_expect_ordered_replies(PoolName, Requests) when is_list(Requests) ->
    batch_call_expect_ordered_replies(PoolName, Requests, ?DEFAULT_TIMEOUT).

-doc """
Processes a list of requests.

Assumes that replies arrive in the order of submitted requests. A batch of
requests may return the same number (or fewer) replies.  If the underlying
protocol is such that a server answers with a subset of replies for a batch of
requests, the requests with `RequestIDs` that don't have replies will contain
`{error, no_reply}`.  When some requests timeout while waiting for the server's
reply, they would return `{error, timeout}`.

When there is a problem calling the server, the function returns `{error,
Reason}`.

Parameters:

- `PoolName` - The name of connection pool
- `Requests` - The list of requests
- `Timeout` - The time period allocated to process the requests

Returns:

A list of replies for each request:
`[{ok, RequestID} | {error, Reason}]`
""".
-spec batch_call_expect_ordered_replies(
        atom(), [request()], timeout_x()) ->
    [{ok, reply()} | {error, no_reply | timeout}] | {error, term()}.
batch_call_expect_ordered_replies(PoolName, Requests, Timeout) when is_list(Requests) ->
    case batch_cast(PoolName, Requests, self(), Timeout) of
        {ok, BatchState} ->
            receive_batch_expect_ordered_replies(BatchState);
        {error, _Reason} = Err ->
            Err
    end.

-doc """
Processes a list of requests.

Parameters:

- `PoolName` - The name of connection pool
- `Requests` - The list of requests

Returns:

The state of the list of requests being processed: `{ok, State} | {errror, Reason}`
""".
-spec batch_cast(shackle_pool:name(), [term()]) ->
    {ok, batch_state()} | {error, atom()}.
batch_cast(PoolName, Requests) when is_list(Requests) ->
    batch_cast(PoolName, Requests, self()).

-doc """
Processes a list of requests.

Parameters:

- `PoolName` - The name of connection pool
- `Requests` - The list of requests
- `Pid` - The process identifier to process the requests

Returns:

The state of the list of requests being processed: `{ok, State} | {errror, Reason}`
""".
-spec batch_cast(shackle_pool:name(), [term()], undefined | pid()) ->
    {ok, batch_state()} | {error, atom()}.
batch_cast(PoolName, Requests, Pid) ->
    batch_cast(PoolName, Requests, Pid, ?DEFAULT_TIMEOUT).

-doc """
Processes a list of requests.

Parameters:

- `PoolName` - The name of connection pool
- `Requests` - The list of requests
- `Pid` - The process identifier to process the requests
- `Timeout` - The time period allocated to process the requests

Returns:

The state of the list of requests being processed: `{ok, State} | {errror, Reason}`
""".
-spec batch_cast(
        shackle_pool:name(), [request()], undefined | pid(), timeout_x()) ->
    {ok, batch_state()} | {error, atom()}.
batch_cast(_, [], _, _) ->
    {ok, {undefined, 0, []}};
batch_cast(PoolName, Requests, Pid, Timeout) when is_list(Requests) ->
    case shackle_pool:options(PoolName) of
        {ok, #pool_options{client = Client} = PoolOpts} ->
            Dispatcher = shackle_observe:dispatcher(),
            Dispatcher:cast(PoolName, Client, fun() ->
                batch_cast_impl(PoolOpts, Requests, Pid, Timeout)
            end);
        {error, Reason} ->
            {error, Reason}
    end.

-spec batch_cast_impl(
        shackle_pool:pool_options(), [request()], undefined | pid(), timeout_x()) ->
    {ok, batch_state()} | {error, atom()}.
batch_cast_impl(#pool_options{} = PoolOpts, Requests, Pid, Timeout) ->
    Timestamp = now_time(),
    Count = length(Requests),
    case shackle_pool:server(PoolOpts, Count) of
        {ok, Client, Server, Sema} ->
            BatchRef = make_ref(),
            CastsRequestRefs = [begin
                #cast{ request_id = {_, RequestRef}} = Cast =
                    mk_cast(BatchRef, Client, Server, Pid, Sema, Timestamp, Timeout),
                {Cast, {RequestRef, Request}}
            end || Request <- Requests],
            {Casts, RequestRefs} = lists:unzip(CastsRequestRefs),
            try
                Server ! {request, [Casts, Requests, Count]},
                {ok, {BatchRef, Count, RequestRefs}}
            catch error:badarg ->
                {error, {bad_server, Server}}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

-doc """
Receive response for the list of requests.

Assumes that replies arrives in the order of submitted requests. Allows requests
not to return replies. For the requests that do not return replies `no_reply` is
returned.

Parameters:

- `BatchState` - The state of the list of requests being processed

Returns:

A list of replies: `[{ok, Reply} | {error, Reason}]`
""".
-spec receive_batch_expect_ordered_replies(batch_state()) ->
    [{ok, reply()} | {error, no_reply | timeout}].
receive_batch_expect_ordered_replies(BatchState) ->
    receive_batch_expect_ordered_replies(BatchState, []).

-doc """
Receive response for the list of requests. Parameters:

- `BatchState` - The state of the list of requests being processed

Returns:

A list of replies: `[{ok, Reply} | {error, Reason}]`
""".
-spec receive_batch_response(batch_state()) ->
    [{request_ref(), reply()} | {error, term()}].
receive_batch_response(BatchState) ->
    receive_batch_response(BatchState, [], infinity).

-spec receive_batch_response(batch_state(), non_neg_integer()|infinity) ->
    [{request_ref(), reply()} | {error, term()}].
receive_batch_response(BatchState, Timeout) when is_integer(Timeout); Timeout==infinity ->
    receive_batch_response(BatchState, [], Timeout).

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
receive_batch_response({_BatchRef, 0, _RequestRefs}, Acc, _Timeout) ->
    lists:reverse(Acc);
receive_batch_response({_BatchRef, _Count, _RequestRefs} = Args, Acc, Timeout) ->
    {Now, Expiration} =
        case Timeout of
            infinity -> {undefined, Timeout};
            _ ->
                N = now_time_msec(),
                {N, N + Timeout}
        end,
    receive_batch_response(Args, Acc, Now, Expiration).

receive_batch_response({_BatchRef, 0, _RequestRefs}, Acc, _Now, _Expiration) ->
    lists:reverse(Acc);
receive_batch_response({BatchRef, Count, RequestRefs}, Acc, Now, Expiration) ->
    Timeout =
        case Expiration of
            infinity ->
                Expiration;
            _ ->
                max(0, Expiration - Now)
        end,
    receive
        {#cast {batch_ref = BatchRef, request_id = {_, RequestRef}}, Reply} ->
            receive_batch_response({BatchRef, Count-1, RequestRefs},
                [{RequestRef, wrap_reply(Reply)} | Acc], now_time_msec(), Expiration)
    after Timeout ->
        Res = lists:foldl(fun({_, RequestRef}, A) ->
            [{RequestRef, {error, timeout}} | A]
        end, Acc, RequestRefs),
        receive_batch_response({BatchRef, 0, []}, Res, Now, Expiration)
    end.

wrap_reply({error, _} = Reply) ->
    Reply;
wrap_reply(Reply) ->
    {ok, Reply}.

receive_batch_expect_ordered_replies({_BatchRef, Count, _RequestRefs}, Acc)
    when Count =< 0 ->
    lists:foldl(fun({_RequestRef, {_Request, Reply}}, ReplyAcc) ->
        [Reply|ReplyAcc] end, [], Acc);
receive_batch_expect_ordered_replies(
    {BatchRef, _Count, RequestRefs} = BatchState, Acc) ->
    [Response] = receive_batch_response({BatchRef, 1, RequestRefs}),
    ProcessedResponse =
        process_ordered_response(Response, RequestRefs, {0, []}),
    {BatchState2, Acc2} =
        handle_ordered_response(ProcessedResponse, BatchState, Acc),
    receive_batch_expect_ordered_replies(BatchState2, Acc2).

process_ordered_response({_RequestRef, _Reply}, [] = _RequestRefReplyPairs,
    {_Count, _Acc}) ->
    %% The case when RequestRef in {RequestRef, Reply}
    %% is not associated with any {RequestRef, Request}.
    {0, [], []};
process_ordered_response({RequestRef, Reply}, [{RequestRef, Request}|T],
    {Count, Acc}) ->
    {Count+1, [{RequestRef, {Request, Reply}}|Acc], T};
process_ordered_response(Ref, [{RequestRef, Request}|T], {Count, Acc}) ->
    process_ordered_response(Ref, T,
        {Count+1, [{RequestRef, {Request, {error, no_reply}}}|Acc]}).

handle_ordered_response({ResponseCount, _, _}, BatchState, Acc)
    when ResponseCount =< 0 ->
    {BatchState, Acc};
handle_ordered_response(_, {_, Count, _} = BatchState, Acc) when Count =< 0 ->
    {BatchState, Acc};
handle_ordered_response({ResponseCount, ResponseAcc, RequestRefsTail},
    {BatchRef, Count, _RequestRefs} = _BatchState, Acc) ->
    {{BatchRef, Count-ResponseCount, RequestRefsTail},
        %% FIXME: need to do this in O(1) rather than O(n)
        lists:append([ResponseAcc, Acc])}.

%% utils
mk_cast(Client, Server, Pid, Sema, Timestamp, Timeout) ->
    mk_cast(undefined, Client, Server, Pid, Sema, Timestamp, Timeout).

mk_cast(BatchRef, Client, Server, Pid0, Sema, Timestamp, Timeout) ->
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
        batch_ref = BatchRef,
        request_id = {Server, make_ref()},
        sema = Sema,    % Semaphore reference to release on cast reply
        timeout = Timeout,
        timestamp = Timestamp
    }.

-spec now_time() -> time().
now_time() ->
    os:system_time(microsecond).

now_time_msec() ->
    os:system_time(millisecond).

%% multi-call public types and functions

-type call_args() ::
    {shackle_pool:name(), request(), proc_func()} |
    {shackle_pool:name(), request(), proc_func(), timeout_x()}.

-type acc() :: term().

% callbacks called to handle result cast result ('send'), or server response ('recv')
-type side() :: send | recv.
-type proc_func() :: fun(
    (side(), reply(), acc()) ->
        {continue, acc()} |
        {chain, call_args() | [call_args()], acc()} |
        {stop, acc()}
).

-spec multi_call([call_args()], timeout_ms(), acc()) -> acc().
multi_call(Calls, Timeout, Acc) ->
    Expiration = expiration(Timeout),
    case multi_cast(Calls, Expiration, Acc) of
        {continue, Acc_, State} ->
            multi_wait(Expiration, Acc_, State);
        {stop, Acc_} ->
            Acc_
    end.

%% private multi-call stuff

-type state() :: #{request_id() => proc_func()}.
-type call_ret() :: {continue, acc(), state()} | {stop, acc()}.
-type expiration() :: {at, pos_integer()}.

-spec multi_cast([call_args()], expiration(), acc()) -> call_ret().
multi_cast(Args, Expiration, Acc) ->
    multi_cast(Args, Expiration, Acc, #{}).

multi_cast([Arg | Args], Expiration, Acc, State) ->
    case place_call(Arg, Expiration, Acc, State) of
        {continue, Acc_, State_} ->
            multi_cast(Args, Expiration, Acc_, State_);
        {stop, Acc_} ->
            {stop, Acc_}
    end;
multi_cast([], _Expiration, Acc, State) ->
    {continue, Acc, State}.

multi_wait(_Expiration, Acc, State) when map_size(State) == 0 ->
    Acc;
multi_wait(Expiration, Acc, State) ->
    receive
        {#cast{request_id = Id}, Reply} ->
            case maps:find(Id, State) of
                {ok, ProcFun} ->
                    case safe_apply(recv, ProcFun, Reply, Expiration, Acc, State) of
                        {continue, Acc_, State_} ->
                            multi_wait(Expiration, Acc_, State_);
                        {stop, Acc_} ->
                            Acc_
                    end;
                error ->
                    multi_wait(Expiration, Acc, State)
            end
    after timeout(Expiration) ->
        Acc
    end.

safe_apply(Side, ProcFun, Data, Expiration, Acc, State) ->
    try ProcFun(Side, Data, Acc) of
        {continue, Acc_} ->
            {continue, Acc_, State};
        {chain, Requests, Acc_} ->
            place_call(Requests, Expiration, Acc_, State);
        {stop, Acc_} ->
            {stop, Acc_};
        Other ->
            ?LOG_ERROR("Unexpected callback return: ~p", [Other]),
            {continue, Acc, State}
    catch
        ?EXCEPTION(C, E, S) ->
            ?LOG_ERROR("Unexpected callback ~p: ~p from ~p", [C, E, ?GET_STACK(S)]),
            {continue, Acc, State}
    end.

place_call({PoolName, Request, ProcFun}, Expiration, Acc, State) when
    is_atom(PoolName), is_function(ProcFun, 3)
->
    place_call({PoolName, Request, ProcFun, infinity}, Expiration, Acc, State);
place_call({PoolName, Request, ProcFun, Timeout}, Expiration, Acc, State) when
    is_atom(PoolName),
    is_function(ProcFun, 3),
    Timeout =:= infinity orelse is_integer(Timeout) andalso Timeout > 0
->
    maybe
        {ok, #pool_options{} = PoolOpts} ?= shackle_pool:options(PoolName),
        {ok, Id} ?= cast_impl(PoolOpts, Request, self(), timeout(Timeout, Expiration), now_time()),
        safe_apply(send, ProcFun, ok, Expiration, Acc, State#{Id => ProcFun})
    else
        {error, Error} ->
            safe_apply(send, ProcFun, {error, Error}, Expiration, Acc, State)
    end;
place_call([Call | Calls], Expiration, Acc, State) ->
    case place_call(Call, Expiration, Acc, State) of
        {continue, Acc_, State_} ->
            place_call(Calls, Expiration, Acc_, State_);
        {stop, Acc_} ->
            {stop, Acc_}
    end;
place_call([], _Expiration, Acc, State) ->
    {continue, Acc, State};
place_call(Args, _Expiration, _Acc, _State) ->
    throw({error, {invalid_args, Args}}).

timeout(infinity, Expiration) -> timeout(Expiration);
timeout(TimeoutMs, _) -> TimeoutMs.

-spec expiration(timeout_ms()) -> expiration().
expiration(TimeoutMs) ->
    {at, erlang:monotonic_time(millisecond) + TimeoutMs}.

-spec timeout(expiration()) -> timeout_ms().
timeout({at, StoptimeMs}) ->
    case StoptimeMs - erlang:monotonic_time(millisecond) of
        TimeoutMs when TimeoutMs > 0 ->
            TimeoutMs;
        _ ->
            0
    end.