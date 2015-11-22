-module(shackle_server).
-include("shackle_internal.hrl").

%% internal
-export([
    init/4,
    start_link/3
]).

%% sys behavior
-export([
    system_code_change/4,
    system_continue/3,
    system_terminate/4
]).

-record(state, {
    client             :: client(),
    client_state       :: term(),
    ip                 :: inet:ip_address() | inet:hostname(),
    name               :: server_name(),
    parent             :: pid(),
    pool_name          :: pool_name(),
    port               :: inet:port_number(),
    protocol           :: protocol(),
    reconnect          :: boolean(),
    reconnect_time_max :: time(),
    reconnect_time_min :: time(),
    reconnect_time     :: time(),
    socket             :: undefined | inet:socket(),
    socket_options     :: [gen_tcp:connect_option()],
    timer_ref          :: undefined | timer:ref()
}).

-type state() :: #state {}.

%% public
-spec start_link(server_name(), pool_name(), client()) -> {ok, pid()}.

start_link(Name, PoolName, Client) ->
    proc_lib:start_link(?MODULE, init, [Name, PoolName, Client, self()]).

-spec init(server_name(), pool_name(), client(), pid()) -> no_return().

init(Name, PoolName, Client, Parent) ->
    process_flag(trap_exit, true),
    proc_lib:init_ack(Parent, {ok, self()}),
    register(Name, self()),

    random:seed(os:timestamp()),
    self() ! ?MSG_CONNECT,
    ok = shackle_backlog:new(Name),
    {ok, Options} = Client:options(),

    Ip = ?LOOKUP(ip, Options, ?DEFAULT_IP),
    Port = ?LOOKUP(port, Options),
    Protocol = ?LOOKUP(protocol, Options, ?DEFAULT_PROTOCOL),
    Reconnect = ?LOOKUP(reconnect, Options, ?DEFAULT_RECONNECT),
    ReconnectTimeMax = ?LOOKUP(reconnect_time_max, Options,
        ?DEFAULT_RECONNECT_MAX),
    ReconnectTimeMin = ?LOOKUP(reconnect_time_min, Options,
        ?DEFAULT_RECONNECT_MIN),
    SocketOptions = ?LOOKUP(socket_options, Options, ?DEFAULT_SOCKET_OPTS),

    loop(#state {
        client = Client,
        ip = Ip,
        name = Name,
        parent = Parent,
        pool_name = PoolName,
        port = Port,
        protocol = Protocol,
        reconnect = Reconnect,
        reconnect_time = ReconnectTimeMin,
        reconnect_time_max = ReconnectTimeMax,
        reconnect_time_min = ReconnectTimeMin,
        socket_options = SocketOptions
    }).

%% sys callbacks
-spec system_code_change(state(), module(), undefined | term(), term()) ->
    {ok, state()}.

system_code_change(State, _Module, _OldVsn, _Extra) ->
    {ok, State}.

-spec system_continue(pid(), [], state()) -> ok.

system_continue(_Parent, _Debug, State) ->
    loop(State).

-spec system_terminate(term(), pid(), [], state()) -> none().

system_terminate(Reason, _Parent, _Debug, _State) ->
    exit(Reason).

%% private
reconnect_time(#state {reconnect = false} = State) ->
    {ok, State#state {
        socket = undefined
    }};
reconnect_time(#state {
        reconnect_time_max = TimeMax,
        reconnect_time = Time
    } = State) ->

    Time2 = shackle_backoff:timeout(Time, TimeMax),
    TimerRef = erlang:send_after(Time2, self(), ?MSG_CONNECT),

    {ok, State#state {
        reconnect_time = Time2,
        socket = undefined,
        timer_ref = TimerRef
    }}.

handle_msg(?MSG_CONNECT, #state {
        protocol = tcp,
        client = Client,
        ip = Ip,
        pool_name = PoolName,
        port = Port,
        reconnect_time_min = TimeMin,
        socket_options = SocketOptions
    } = State) ->

    case gen_tcp:connect(Ip, Port, SocketOptions) of
        {ok, Socket} ->
            {ok, ClientOptions} = Client:options(),
            ClientState = ?LOOKUP(state, ClientOptions),
            inet:setopts(Socket, [{active, false}]),

            case Client:setup(Socket, ClientState) of
                {ok, ClientState2} ->
                    inet:setopts(Socket, [{active, true}]),

                    {ok, State#state {
                        client_state = ClientState2,
                        socket = Socket,
                        reconnect_time = TimeMin
                    }};
                {error, Reason, ClientState2} ->
                    shackle_utils:warning_msg(PoolName,
                        "setup error: ~p", [Reason]),

                    reconnect_time(State#state {
                        client_state = ClientState2
                    })
            end;
        {error, Reason} ->
            shackle_utils:warning_msg(PoolName,
                "tcp connect error: ~p", [Reason]),
            reconnect_time(State)
    end;
handle_msg(?MSG_CONNECT, #state {
        protocol = udp,
        client = Client,
        pool_name = PoolName,
        % port = Port,
        reconnect_time_min = TimeMin,
        socket_options = SocketOptions
    } = State) ->

    case gen_udp:open(0, SocketOptions) of
        {ok, Socket} ->
            {ok, ClientOptions} = Client:options(),
            ClientState = ?LOOKUP(state, ClientOptions),
            inet:setopts(Socket, [{active, false}]),

            case Client:setup(Socket, ClientState) of
                {ok, ClientState2} ->
                    inet:setopts(Socket, [{active, true}]),

                    {ok, State#state {
                        client_state = ClientState2,
                        socket = Socket,
                        reconnect_time = TimeMin
                    }};
                {error, Reason, ClientState2} ->
                    shackle_utils:warning_msg(PoolName,
                        "setup error: ~p", [Reason]),

                    reconnect_time(State#state {
                        client_state = ClientState2
                    })
            end;
        {error, Reason} ->
            shackle_utils:warning_msg(PoolName,
                "udp connect error: ~p", [Reason]),
            reconnect_time(State)
    end;
handle_msg(#cast {} = Cast, #state {
        socket = undefined,
        name = Name
    } = State) ->

    reply(Name, {error, no_socket}, Cast),
    {ok, State};
handle_msg(#cast {
        request = Request,
        timestamp = Timestamp,
        timing = Timing
    } = Cast, #state {
        protocol = tcp,
        client = Client,
        client_state = ClientState,
        pool_name = PoolName,
        name = Name,
        socket = Socket
    } = State) ->

    {ok, ExtRequestId, Data, ClientState2} =
        Client:handle_request(Request, ClientState),

    case gen_tcp:send(Socket, Data) of
        ok ->
            shackle_queue:in(Name, ExtRequestId, Cast#cast {
                timing = shackle_utils:timing(Timestamp, Timing)
            }),

            {ok, State#state {
                client_state = ClientState2
            }};
        {error, Reason} ->
            shackle_utils:warning_msg(PoolName, "tcp send error: ~p", [Reason]),
            gen_tcp:close(Socket),
            tcp_close(State)
    end;
handle_msg(#cast {
        request = Request,
        timestamp = Timestamp,
        timing = Timing
    } = Cast, #state {
        protocol = udp,
        client = Client,
        client_state = ClientState,
        ip = Ip,
        name = Name,
        pool_name = PoolName,
        port = Port,
        socket = Socket
    } = State) ->

    {ok, ExtRequestId, Data, ClientState2} =
        Client:handle_request(Request, ClientState),

    case gen_udp:send(Socket, Ip, Port, Data) of
        ok ->
            shackle_queue:in(Name, ExtRequestId, Cast#cast {
                timing = shackle_utils:timing(Timestamp, Timing)
            }),

            {ok, State#state {
                client_state = ClientState2
            }};
        {error, Reason} ->
            shackle_utils:warning_msg(PoolName, "udp send error: ~p", [Reason]),
            gen_udp:close(Socket),
            tcp_close(State)
    end;
handle_msg({tcp, _Port, Data}, #state {
        protocol = tcp,
        client = Client,
        client_state = ClientState,
        pool_name = PoolName,
        name = Name
    } = State) ->

    {ok, Replies, ClientState2} = Client:handle_data(Data, ClientState),

    lists:foreach(fun ({ExtRequestId, Reply}) ->
        case shackle_queue:out(Name, ExtRequestId) of
            {ok, #cast {
                timestamp = Timestamp,
                timing = Timing
            } = Cast} ->

                reply(Name, Reply, Cast#cast {
                    timing = shackle_utils:timing(Timestamp, Timing)
                });
            {error, not_found} ->
                shackle_utils:info_msg(PoolName,
                    "shackle_queue not found: ~p", [ExtRequestId])
        end
    end, Replies),

    {ok, State#state {
        client_state = ClientState2
    }};
handle_msg({tcp_closed, Socket}, #state {
        protocol = tcp,
        socket = Socket,
        pool_name = PoolName
    } = State) ->

    shackle_utils:warning_msg(PoolName, "tcp connection closed", []),
    tcp_close(State);
handle_msg({tcp_error, Socket, Reason}, #state {
        protocol = tcp,
        socket = Socket,
        pool_name = PoolName
    } = State) ->

    shackle_utils:warning_msg(PoolName, "tcp connection error: ~p", [Reason]),
    gen_tcp:close(Socket),
    tcp_close(State);
handle_msg({udp, _Socket, _Ip, _InPortNo, Data}, #state {
        protocol = udp,
        client = Client,
        client_state = ClientState,
        pool_name = PoolName,
        name = Name
    } = State) ->

    {ok, Replies, ClientState2} = Client:handle_data(Data, ClientState),

    lists:foreach(fun ({ExtRequestId, Reply}) ->
        case shackle_queue:out(Name, ExtRequestId) of
            {ok, #cast {
                timestamp = Timestamp,
                timing = Timing
            } = Cast} ->

                reply(Name, Reply, Cast#cast {
                    timing = shackle_utils:timing(Timestamp, Timing)
                });
            {error, not_found} ->
                shackle_utils:info_msg(PoolName,
                    "shackle_queue not found: ~p", [ExtRequestId])
        end
    end, Replies),

    {ok, State#state {
        client_state = ClientState2
    }}.

loop(#state {parent = Parent} = State) ->
    receive
        {'EXIT', Parent, Reason} ->
            terminate(Reason, State);
        {system, From, Request} ->
            sys:handle_system_msg(Request, From, Parent, ?MODULE, [], State);
        Msg ->
            {ok, State2} = handle_msg(Msg, State),
            loop(State2)
    end.

reply(Name, Reply, Cast) ->
    shackle_backlog:decrement(Name),
    Cast#cast.pid ! Cast#cast {
        reply = Reply
    }.

reply_all(Name, Reply) ->
    Requests = shackle_queue:all(Name),
    [reply(Name, Reply, Request) || Request <- Requests].

tcp_close(#state {name = Name} = State) ->
    reply_all(Name, {error, tcp_closed}),
    reconnect_time(State).

terminate(Reason, #state {
        client = Client,
        client_state = ClientState,
        name = Name,
        timer_ref = TimerRef
    }) ->

    shackle_utils:cancel_timer(TimerRef),
    ok = Client:terminate(ClientState),
    reply_all(Name, {error, shutdown}),
    ok = shackle_backlog:delete(Name),
    exit(Reason).
