-module(shackle_server).
-include("shackle_internal.hrl").

-compile(inline).
-compile({inline_size, 512}).

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
    backlog          :: shackle:table(),
    client           :: shackle:client(),
    id               :: id(),
    init_options     :: init_options(),
    name             :: name(),
    parent           :: pid(),
    pool_name        :: shackle_pool:name(),
    port             :: shackle:inet_port(),
    protocol         :: shackle:protocol(),
    queue            :: shackle:table(),
    reconnect_state  :: undefined | reconnect_state(),
    socket           :: undefined | shackle:socket(),
    socket_options   :: shackle:socket_options(),
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
    ok = shackle_backlog:new(PoolName, Id),

    InitOptions = ?LOOKUP(init_options, ClientOptions, ?DEFAULT_INIT_OPTS),
    Address = address(ClientOptions),
    Port = ?LOOKUP(port, ClientOptions),
    Protocol = ?LOOKUP(protocol, ClientOptions, ?DEFAULT_PROTOCOL),
    ReconnectState = reconnect_state(ClientOptions),
    SocketOptions = ?LOOKUP(socket_options, ClientOptions,
        ?DEFAULT_SOCKET_OPTS),

    {ok, {#state {
        address = Address,
        backlog = shackle_backlog:table_name(PoolName),
        client = Client,
        id = Id,
        init_options = InitOptions,
        name = Name,
        parent = Parent,
        pool_name = PoolName,
        port = Port,
        protocol = Protocol,
        queue = shackle_queue:table_name(PoolName),
        reconnect_state = ReconnectState,
        socket_options = SocketOptions
    }, undefined}}.

-spec handle_msg(term(), {state(), client_state()}) ->
    {ok, term()}.

handle_msg({_, #cast {} = Cast}, {#state {
        socket = undefined
    } = State, ClientState}) ->

    reply({error, no_socket}, Cast, State),
    {ok, {State, ClientState}};
handle_msg({Request, #cast {
        timeout = Timeout
    } = Cast}, {#state {
        client = Client,
        id = Id,
        pool_name = PoolName,
        protocol = Protocol,
        queue = Queue,
        socket = Socket
    } = State, ClientState}) ->

    try Client:handle_request(Request, ClientState) of
        {ok, ExtRequestId, Data, ClientState2} ->
            case Protocol:send(Socket, Data) of
                ok ->
                    shackle_telemetry:send(Client, iolist_size(Data)),
                    case ExtRequestId of
                        undefined ->
                            reply(ok, Cast, State);
                        _ ->
                            Msg = {timeout, ExtRequestId},
                            TimerRef = erlang:send_after(Timeout, self(), Msg),
                            shackle_queue:add(Queue, Id, ExtRequestId, Cast,
                                TimerRef)
                    end,
                    {ok, {State, ClientState2}};
                {error, Reason} ->
                    ?WARN(PoolName, "send error: ~p", [Reason]),
                    Protocol:close(Socket),
                    reply({error, socket_closed}, Cast, State),
                    close(State, ClientState2)
            end
    catch
        ?EXCEPTION(E, R, Stacktrace) ->
            ?WARN(PoolName, "handle_request crash: ~p:~p~n~p~n",
                [E, R, ?GET_STACK(Stacktrace)]),
            reply({error, client_crash}, Cast, State),
            {ok, {State, ClientState}}
    end;
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
                        socket = Socket
                    }, ClientState2}};
                {error, _Reason, ClientState2} ->
                    Protocol:close(Socket),
                    reconnect(State, ClientState2)
            end;
        {error, _Reason} ->
            reconnect(State, ClientState)
    end;
handle_msg({timeout, ExtRequestId}, {#state {
        client = Client,
        id = Id,
        pool_name = PoolName,
        protocol = Protocol,
        queue = Queue,
        socket = Socket
    } = State, ClientState}) ->

    case erlang:function_exported(Client, handle_timeout, 2) of
        true ->
            try Client:handle_timeout(ExtRequestId, ClientState) of
                {ok, Reply, ClientState2} ->
                    shackle_telemetry:handle_timeout(Client),
                    process_responses([Reply], State),
                    {ok, {State, ClientState2}};
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
            case shackle_queue:remove(Queue, Id, ExtRequestId) of
                {ok, Cast, _TimerRef} ->
                    shackle_telemetry:timeout(Client),
                    reply({error, timeout}, Cast, State);
                {error, not_found} ->
                    ok
            end,
            {ok, {State, ClientState}}
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
        timer_ref = TimerRef
    } = State, ClientState}) ->

    cancel_timer(TimerRef),
    try Client:terminate(ClientState)
    catch
        ?EXCEPTION(E, R, Stacktrace) ->
            ?WARN(PoolName, "terminate crash: ~p:~p~n~p~n",
                [E, R, ?GET_STACK(Stacktrace)])
    end,
    reply_all({error, shutdown}, State),
    shackle_backlog:delete(PoolName, Id).

%% private
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
    reply_all({error, socket_closed}, State),
    reconnect(State, ClientState).

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
        socket = Socket
    } = State, ClientState) ->

    shackle_telemetry:recv(Client, size(Data)),
    try Client:handle_data(Data, ClientState) of
        {ok, Replies, ClientState2} ->
            process_responses(Replies, State),
            {ok, {State, ClientState2}};
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

process_responses([], _State) ->
    ok;
process_responses([{ExtRequestId, Reply} | T], #state {
        client = Client,
        id = Id,
        queue = Queue
    } = State) ->

    shackle_telemetry:replies(Client),
    case shackle_queue:remove(Queue, Id, ExtRequestId) of
        {ok, #cast {timestamp = Timestamp} = Cast, TimerRef} ->
            shackle_telemetry:found(Client),
            Diff = timer:now_diff(os:timestamp(), Timestamp),
            shackle_telemetry:reply(Client, Diff),
            erlang:cancel_timer(TimerRef),
            reply(Reply, Cast, State);
        {error, not_found} ->
            shackle_telemetry:not_found(Client),
            ok
    end,
    process_responses(T, State).

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
reply(Reply, #cast {pid = Pid} = Cast, #state {
        backlog = Backlog,
        id = Id
    }) ->

    shackle_backlog:decrement(Backlog, Id),
    Pid ! {Cast, Reply},
    ok.

reply_all(Reply, #state {
        id = Id,
        queue = Queue
    } = State) ->

    Requests = shackle_queue:clear(Queue, Id),
    reply_all(Reply, Requests, State).

reply_all(_Reply, [], _State) ->
    ok;
reply_all(Reply, [{Cast, TimerRef} | T], State) ->
    erlang:cancel_timer(TimerRef),
    reply(Reply, Cast, State),
    reply_all(Reply, T, State).
