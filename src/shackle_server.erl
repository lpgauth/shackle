-module(shackle_server).
-include("shackle_internal.hrl").

-export([
    start_link/2
]).

-behavior(metal).
-export([
    init/3,
    handle_msg/2,
    terminate/2
]).

-record(state, {
    address          :: shackle:inet_address(),
    backlog          :: atomics:atomics_ref(),
    client           :: shackle:client(),
    id               :: id(),
    init_options     :: init_options(),
    max_requests     :: pos_integer() | infinity,
    name             :: name(),
    parent           :: pid(),
    pool_name        :: shackle_pool:name(),
    port             :: shackle:inet_port(),
    protocol         :: shackle:protocol(),
    queue = #{}      :: #{shackle:external_request_id() =>
                              {shackle:cast(), integer()}},
    reconnect_state  :: undefined | reconnect_state(),
    requests_left    :: non_neg_integer() | infinity,
    socket           :: undefined | shackle:socket(),
    socket_options   :: shackle:socket_options(),
    sweep_deadline = 0 :: integer(),
    sweep_ref        :: undefined | reference(),
    telemetry = true :: boolean(),
    timer_ref        :: undefined | reference()

}).

-type state() :: #state {}.
-type client_state() :: term().
-type init_options() :: term().
-type id() :: {shackle_pool:name(), index()}.
-type index() :: pos_integer().
-type name() :: atom().
-type opts() :: {shackle_pool:name(), index(), shackle:client(), shackle_client:options()}.
-type reconnect_state() :: #reconnect_state{}.

-export_type([
    id/0,
    init_options/0,
    name/0,
    reconnect_state/0
]).

%% public
-spec start_link(name(), opts()) ->
    {ok, pid()}.

start_link(Name, Opts) ->
    metal:start_link(?MODULE, Name, Opts).

%% metal callbacks
-spec init(name(), pid(), opts()) ->
    no_return().

init(Name, Parent, Opts) ->
    {PoolName, Index, Client, ClientOptions} = Opts,
    self() ! ?MSG_CONNECT,
    Id = {PoolName, Index},
    ok = shackle_backlog:reset(PoolName, Id),

    InitOptions = ?LOOKUP(init_options, ClientOptions, ?DEFAULT_INIT_OPTS),
    Address = address(ClientOptions),
    MaxRequests = ?LOOKUP(max_requests, ClientOptions,
        ?DEFAULT_MAX_REQUESTS),
    Port = ?LOOKUP(port, ClientOptions),
    Protocol = ?LOOKUP(protocol, ClientOptions, ?DEFAULT_PROTOCOL),
    ReconnectState = reconnect_state(ClientOptions),
    SocketOptions = ?LOOKUP(socket_options, ClientOptions,
        ?DEFAULT_SOCKET_OPTS),

    {ok, {#state {
        address = Address,
        backlog = shackle_backlog:ref(PoolName),
        client = Client,
        id = Id,
        init_options = InitOptions,
        max_requests = MaxRequests,
        name = Name,
        parent = Parent,
        pool_name = PoolName,
        port = Port,
        protocol = Protocol,
        reconnect_state = ReconnectState,
        requests_left = MaxRequests,
        socket_options = SocketOptions,
        telemetry = persistent_term:get({shackle, telemetry}, true)
    }, undefined}}.

-spec handle_msg(term(), {state(), client_state()}) ->
    {ok, term()}.

handle_msg({_, #cast {} = Cast}, {#state {
        socket = undefined
    } = State, ClientState}) ->

    reply({error, no_socket}, Cast, State),
    {ok, {State, ClientState}};
handle_msg({Request, #cast {} = Cast}, {#state {
        protocol = shackle_udp
    } = State, ClientState}) ->

    handle_casts([{Request, Cast}], State, ClientState);
handle_msg({Request, #cast {} = Cast}, {State, ClientState}) ->
    RequestCasts = drain_casts([{Request, Cast}], ?MAX_CAST_BATCH - 1),
    handle_casts(RequestCasts, State, ClientState);
handle_msg({'$socket', Socket, select, _Handle}, {#state {
        socket = Socket
    } = State, ClientState}) ->

    case shackle_socket:recv(Socket) of
        {ok, Data} ->
            handle_msg_data(Socket, Data, State, ClientState);
        wait ->
            {ok, {State, ClientState}};
        {error, closed} ->
            handle_msg_close(Socket, State, ClientState);
        {error, Reason} ->
            handle_msg_error(Socket, Reason, State, ClientState)
    end;
handle_msg({'$socket', _Socket, select, _Handle}, {State, ClientState}) ->
    {ok, {State, ClientState}};
handle_msg({'$socket', Socket, abort, _Info}, {State, ClientState}) ->
    handle_msg_close(Socket, State, ClientState);
handle_msg({ssl, Socket, Data}, {State, ClientState}) ->
    handle_msg_data(Socket, Data, State, ClientState);
handle_msg({ssl_closed, Socket}, {State, ClientState}) ->
    handle_msg_close(Socket, State, ClientState);
handle_msg({ssl_error, Socket, Reason}, {State, ClientState}) ->
    handle_msg_error(Socket, Reason, State, ClientState);
handle_msg({tcp, Socket, Data}, {State, ClientState}) ->
    handle_msg_data(Socket, Data, State, ClientState);
handle_msg({tcp_closed, Socket}, {State, ClientState}) ->
    handle_msg_close(Socket, State, ClientState);
handle_msg({tcp_error, Socket, Reason}, {State, ClientState}) ->
    handle_msg_error(Socket, Reason, State, ClientState);
handle_msg({udp, Socket, _Ip, _InPortNo, Data}, {State, ClientState}) ->
    handle_msg_data(Socket, Data, State, ClientState);
handle_msg({udp_error, Socket, Reason}, {State, ClientState}) ->
    handle_msg_error(Socket, Reason, State, ClientState);
handle_msg(?MSG_CONNECT, {#state {
        address = Address,
        client = Client,
        id = Id,
        init_options = Init,
        max_requests = MaxRequests,
        pool_name = PoolName,
        port = Port,
        protocol = Protocol,
        reconnect_state = ReconnectState,
        socket_options = SocketOptions
    } = State, ClientState}) ->

    case connect(Protocol, Address, Port, SocketOptions, PoolName) of
        {ok, Socket} ->
            case client(Client, PoolName, Init, Protocol, Socket) of
                {ok, ClientState2} ->
                    ReconnectState2 = reconnect_state_reset(ReconnectState),
                    shackle_status:enable(Id),

                    {ok, {State#state {
                        reconnect_state = ReconnectState2,
                        requests_left = MaxRequests,
                        socket = Socket
                    }, ClientState2}};
                {error, _Reason, ClientState2} ->
                    Protocol:close(Socket),
                    reconnect(State, ClientState2)
            end;
        {error, _Reason} ->
            reconnect(State, ClientState)
    end;
handle_msg(?MSG_SWEEP, {#state {
        queue = Queue
    } = State, ClientState}) when map_size(Queue) =:= 0 ->

    {ok, {State#state {sweep_ref = undefined}, ClientState}};
handle_msg(?MSG_SWEEP, {#state {
        queue = Queue
    } = State, ClientState}) ->

    Now = erlang:monotonic_time(millisecond),
    {Expired, NextDeadline} = maps:fold(fun
        (ExtRequestId, {_Cast, Deadline}, {ExpiredAcc, MinAcc})
                when Deadline =< Now ->
            {[ExtRequestId | ExpiredAcc], MinAcc};
        (_ExtRequestId, {_Cast, Deadline}, {ExpiredAcc, MinAcc}) ->
            {ExpiredAcc, min(Deadline, MinAcc)}
    end, {[], infinity}, Queue),

    case expire(Expired, State#state {sweep_ref = undefined}, ClientState) of
        {ok, {State2, ClientState2}} when is_integer(NextDeadline) ->
            {ok, {arm_sweep(Now, NextDeadline, State2), ClientState2}};
        {ok, {State2, ClientState2}} ->
            maybe_recycle(State2, ClientState2)
    end;
handle_msg(Msg, {#state {
        pool_name = PoolName
    } = State, ClientState}) ->

    ?WARN(PoolName, "unknown msg: ~p", [Msg]),
    {ok, {State, ClientState}}.

-spec terminate(term(), term()) ->
    ok.

terminate(_Reason, {#state {
        client = Client,
        id = Id,
        pool_name = PoolName,
        sweep_ref = SweepRef,
        timer_ref = TimerRef
    } = State, ClientState}) ->

    cancel_timer(SweepRef),
    cancel_timer(TimerRef),
    try Client:terminate(ClientState)
    catch
        ?EXCEPTION(E, R, Stacktrace) ->
            ?WARN(PoolName, "terminate crash: ~p:~p~n~p~n",
                [E, R, ?GET_STACK(Stacktrace)])
    end,
    reply_all({error, shutdown}, State),
    shackle_backlog:reset(PoolName, Id).

%% private
%% A single armed timer covers the earliest deadline in the queue;
%% replies never cancel it, so an idle fire sweeps nothing and
%% re-arms to the new minimum.
arm_sweep(Now, Deadline, #state {sweep_ref = undefined} = State) ->
    SweepRef = erlang:send_after(max(0, Deadline - Now), self(), ?MSG_SWEEP),
    State#state {
        sweep_deadline = Deadline,
        sweep_ref = SweepRef
    };
arm_sweep(_Now, Deadline, #state {
        sweep_deadline = Armed
    } = State) when Deadline >= Armed ->

    State;
arm_sweep(Now, Deadline, #state {sweep_ref = SweepRef} = State) ->
    erlang:cancel_timer(SweepRef),
    arm_sweep(Now, Deadline, State#state {sweep_ref = undefined}).

%% Casts already sitting in the mailbox are drained and written with
%% a single send; socket messages behind them were queued after the
%% casts, so replies are never reordered ahead of their requests.
drain_casts(Acc, 0) ->
    lists:reverse(Acc);
drain_casts(Acc, N) ->
    receive
        {Request, #cast {} = Cast} ->
            drain_casts([{Request, Cast} | Acc], N - 1)
    after 0 ->
        lists:reverse(Acc)
    end.

encode_casts([], _State, ClientState, Entries, Data) ->
    {lists:reverse(Entries), lists:reverse(Data), ClientState};
encode_casts([{Request, Cast} | T], #state {
        client = Client,
        pool_name = PoolName
    } = State, ClientState, Entries, Data) ->

    try Client:handle_request(Request, ClientState) of
        {ok, ExtRequestId, RequestData, ClientState2} ->
            encode_casts(T, State, ClientState2,
                [{Cast, ExtRequestId} | Entries], [RequestData | Data])
    catch
        ?EXCEPTION(E, R, Stacktrace) ->
            ?WARN(PoolName, "handle_request crash: ~p:~p~n~p~n",
                [E, R, ?GET_STACK(Stacktrace)]),
            reply({error, client_crash}, Cast, State),
            encode_casts(T, State, ClientState, Entries, Data)
    end.

enqueue_casts([], _Now, State) ->
    State;
enqueue_casts([{Cast, undefined} | T], Now, State) ->
    reply(ok, Cast, State),
    enqueue_casts(T, Now, State);
enqueue_casts([{#cast {timeout = Timeout} = Cast, ExtRequestId} | T], Now,
        #state {queue = Queue} = State) ->

    Deadline = Now + Timeout,
    Queue2 = maps:put(ExtRequestId, {Cast, Deadline}, Queue),
    State2 = arm_sweep(Now, Deadline, State#state {queue = Queue2}),
    enqueue_casts(T, Now, State2).

handle_casts(RequestCasts, #state {
        client = Client,
        pool_name = PoolName,
        protocol = Protocol,
        socket = Socket,
        telemetry = Telemetry
    } = State, ClientState) ->

    {Entries, Data, ClientState2} =
        encode_casts(RequestCasts, State, ClientState, [], []),
    case Entries of
        [] ->
            {ok, {State, ClientState2}};
        _ ->
            case Protocol:send(Socket, Data) of
                ok ->
                    Telemetry andalso
                        shackle_telemetry:send(Client, iolist_size(Data)),
                    Now = erlang:monotonic_time(millisecond),
                    State2 = enqueue_casts(Entries, Now, State),
                    State3 = requests_sent(length(Entries), State2),
                    maybe_recycle(State3, ClientState2);
                {error, Reason} ->
                    ?WARN(PoolName, "send error: ~p", [Reason]),
                    Protocol:close(Socket),
                    [reply({error, socket_closed}, Cast, State) ||
                        {Cast, _ExtRequestId} <- Entries],
                    close(State, ClientState2)
            end
    end.

expire([], State, ClientState) ->
    {ok, {State, ClientState}};
expire([ExtRequestId | T], #state {
        client = Client,
        pool_name = PoolName,
        protocol = Protocol,
        queue = Queue,
        socket = Socket,
        telemetry = Telemetry
    } = State, ClientState) ->

    case erlang:function_exported(Client, handle_timeout, 2) of
        true ->
            try Client:handle_timeout(ExtRequestId, ClientState) of
                {ok, Reply, ClientState2} ->
                    Telemetry andalso shackle_telemetry:handle_timeout(Client),
                    State2 = process_responses([Reply], State),
                    expire(T, State2, ClientState2);
                {error, Reason, ClientState2} ->
                    ?WARN(PoolName, "handle_timeout error: ~p", [Reason]),
                    Protocol:close(Socket),
                    close(State, ClientState2)
            catch
                ?EXCEPTION(E, R, Stacktrace) ->
                    ?WARN(PoolName, "handle_timeout error: ~p:~p~n~p~n",
                        [E, R, ?GET_STACK(Stacktrace)]),
                    Protocol:close(Socket),
                    close(State, ClientState)
            end;
        false ->
            case maps:take(ExtRequestId, Queue) of
                {{Cast, _Deadline}, Queue2} ->
                    Telemetry andalso shackle_telemetry:timeout(Client),
                    reply({error, timeout}, Cast, State),
                    expire(T, State#state {queue = Queue2}, ClientState);
                error ->
                    expire(T, State, ClientState)
            end
    end.

address(ClientOptions) ->
    case ?LOOKUP(address, ClientOptions) of
        undefined ->
            ?LOOKUP(ip, ClientOptions, ?DEFAULT_ADDRESS);
        Address ->
            Address
    end.

cancel_timer(undefined) ->
    ok;
cancel_timer(TimerRef) ->
    erlang:cancel_timer(TimerRef).

client(Client, PoolName, InitOptions, Protocol, Socket) ->
    case client_init(Client, PoolName, InitOptions) of
        {ok, ClientState} ->
            client_setup(Client, PoolName, Protocol, Socket, ClientState);
        {error, Reason} ->
            {error, Reason, undefined}
    end.

client_init(Client, PoolName, InitOptions) ->
    try Client:init(InitOptions) of
        {ok, ClientState} ->
            {ok, ClientState};
        {error, Reason} ->
            ?WARN(PoolName, "init error: ~p~n", [Reason]),
            {error, Reason}
    catch
        ?EXCEPTION(E, R, Stacktrace) ->
            ?WARN(PoolName, "init crash: ~p:~p~n~p~n",
                [E, R, ?GET_STACK(Stacktrace)]),
            {error, client_crash}
    end.

client_setup(Client, PoolName, Protocol, Socket, ClientState) ->
    Protocol:setopts(Socket, [{active, false}]),
    try Client:setup(Socket, ClientState) of
        {ok, ClientState2} ->
            Protocol:setopts(Socket, [{active, true}]),
            {ok, ClientState2};
        {error, Reason, ClientState2} ->
            ?WARN(PoolName, "setup error: ~p", [Reason]),
            {error, Reason, ClientState2}
    catch
        ?EXCEPTION(E, R, Stacktrace) ->
            ?WARN(PoolName, "handle_data error: ~p:~p~n~p~n",
                [E, R, ?GET_STACK(Stacktrace)]),
            {error, client_crash, ClientState}
    end.

close(#state {id = Id} = State, ClientState) ->
    shackle_status:disable(Id),
    State2 = reply_all({error, socket_closed}, State),
    reconnect(State2, ClientState).

connect(Protocol, Address, Port, SocketOptions, PoolName) ->
    case inet:getaddrs(Address, inet) of
        {ok, Ips} ->
            Ip = shackle_utils:random_element(Ips),
            case Protocol:connect(Ip, Port, SocketOptions) of
                {ok, Socket} ->
                    {ok, Socket};
                {error, Reason} ->
                    ?WARN(PoolName, "connect error: ~p", [Reason]),
                    {error, Reason}
            end;
        {error, Reason} ->
            ?WARN(PoolName, "getaddrs error: ~p", [Reason]),
            {error, Reason}
    end.

handle_msg_close(Socket, #state {
        socket = Socket,
        pool_name = PoolName
    } = State, ClientState) ->

    ?DEBUG(PoolName, "connection closed", []),
    close(State, ClientState);
handle_msg_close(_Socket, State, ClientState) ->
    {ok, {State, ClientState}}.

handle_msg_data(Socket, Data, #state {
        client = Client,
        pool_name = PoolName,
        protocol = Protocol,
        socket = Socket,
        telemetry = Telemetry
    } = State, ClientState) ->

    Telemetry andalso shackle_telemetry:recv(Client, size(Data)),
    try Client:handle_data(Data, ClientState) of
        {ok, Replies, ClientState2} ->
            State2 = process_responses(Replies, State),
            maybe_recycle(State2, ClientState2);
        {error, Reason, ClientState2} ->
            ?WARN(PoolName, "handle_data error: ~p", [Reason]),
            Protocol:close(Socket),
            close(State, ClientState2)
    catch
        ?EXCEPTION(E, R, Stacktrace) ->
            ?WARN(PoolName, "handle_data crash: ~p:~p~n~p~n",
                [E, R, ?GET_STACK(Stacktrace)]),
            Protocol:close(Socket),
            close(State, ClientState)
    end;
handle_msg_data(_Socket, _Data, State, ClientState) ->
    {ok, {State, ClientState}}.

handle_msg_error(Socket, Reason, #state {
        socket = Socket,
        pool_name = PoolName,
        protocol = Protocol
    } = State, ClientState) ->

    ?WARN(PoolName, "connection error: ~p", [Reason]),
    Protocol:close(Socket),
    close(State, ClientState);
handle_msg_error(_Socket, _Reason, State, ClientState) ->
    {ok, {State, ClientState}}.

maybe_recycle(#state {
        client = Client,
        pool_name = PoolName,
        protocol = Protocol,
        queue = Queue,
        requests_left = 0,
        socket = Socket
    } = State, ClientState) when map_size(Queue) =:= 0,
        Socket =/= undefined ->

    ?DEBUG(PoolName, "max_requests reached, recycling connection", []),
    Protocol:close(Socket),
    try Client:terminate(ClientState)
    catch
        ?EXCEPTION(E, R, Stacktrace) ->
            ?WARN(PoolName, "terminate crash: ~p:~p~n~p~n",
                [E, R, ?GET_STACK(Stacktrace)])
    end,
    self() ! ?MSG_CONNECT,
    {ok, {State#state {socket = undefined}, ClientState}};
maybe_recycle(State, ClientState) ->
    {ok, {State, ClientState}}.

process_responses([], State) ->
    State;
process_responses([{ExtRequestId, Reply} | T], #state {
        client = Client,
        queue = Queue,
        telemetry = Telemetry
    } = State) ->

    Telemetry andalso shackle_telemetry:replies(Client),
    case maps:take(ExtRequestId, Queue) of
        {{#cast {timestamp = Timestamp} = Cast, _Deadline}, Queue2} ->
            case Telemetry of
                true ->
                    shackle_telemetry:found(Client),
                    Diff = erlang:monotonic_time(microsecond) - Timestamp,
                    shackle_telemetry:reply(Client, Diff);
                false ->
                    ok
            end,
            reply(Reply, Cast, State),
            process_responses(T, State#state {queue = Queue2});
        error ->
            Telemetry andalso shackle_telemetry:not_found(Client),
            process_responses(T, State)
    end.

reconnect(State, undefined) ->
    reconnect_timer(State, undefined);
reconnect(#state {
        client = Client,
        pool_name = PoolName
    } = State, ClientState) ->

    try Client:terminate(ClientState)
    catch
        ?EXCEPTION(E, R, Stacktrace) ->
            ?WARN(PoolName, "terminate crash: ~p:~p~n~p~n",
                [E, R, ?GET_STACK(Stacktrace)])
    end,
    reconnect_timer(State, ClientState).

reconnect_state(Options) ->
    Reconnect = ?LOOKUP(reconnect, Options, ?DEFAULT_RECONNECT),
    case Reconnect of
        true ->
            Max = ?LOOKUP(reconnect_time_max, Options,
                ?DEFAULT_RECONNECT_MAX),
            Min = ?LOOKUP(reconnect_time_min, Options,
                ?DEFAULT_RECONNECT_MIN),

            #reconnect_state {
                min = Min,
                max = Max
            };
        false ->
            undefined
    end.

reconnect_state_reset(undefined) ->
    undefined;
reconnect_state_reset(#reconnect_state {} = ReconnectState) ->
    ReconnectState#reconnect_state {
        current = undefined
    }.

reconnect_timer(#state {
        reconnect_state = undefined
    } = State, ClientState) ->

    {ok, {State#state {
        socket = undefined
    }, ClientState}};
reconnect_timer(#state {
        reconnect_state = ReconnectState
    } = State, ClientState)  ->

    ReconnectState2 = shackle_backoff:timeout(ReconnectState),
    #reconnect_state {current = Current} = ReconnectState2,
    TimerRef = erlang:send_after(Current, self(), ?MSG_CONNECT),

    {ok, {State#state {
        reconnect_state = ReconnectState2,
        socket = undefined,
        timer_ref = TimerRef
    }, ClientState}}.

reply(_Reply, #cast {pid = undefined}, #state {
        backlog = Backlog,
        id = Id
    }) ->

    shackle_backlog:decrement(Backlog, Id),
    ok;
reply(Reply, #cast {pid = Pid, request_id = RequestId}, #state {
        backlog = Backlog,
        id = Id
    }) ->

    shackle_backlog:decrement(Backlog, Id),
    Pid ! {shackle_reply, RequestId, Reply},
    ok.

reply_all(Reply, #state {queue = Queue} = State) ->
    maps:foreach(fun (_ExtRequestId, {Cast, _Deadline}) ->
        reply(Reply, Cast, State)
    end, Queue),
    State#state {queue = #{}}.

%% Disabling at the cap keeps new requests off this server so the
%% in-flight tail can drain; the recycle then reconnects immediately.
requests_sent(_N, #state {requests_left = infinity} = State) ->
    State;
requests_sent(N, #state {requests_left = Left} = State) when Left > N ->
    State#state {requests_left = Left - N};
requests_sent(_N, #state {requests_left = 0} = State) ->
    State;
requests_sent(_N, #state {id = Id} = State) ->
    shackle_status:disable(Id),
    State#state {requests_left = 0}.
