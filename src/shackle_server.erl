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
    connect_options    :: [gen_tcp:connect_option()],
    ip                 :: inet:ip_address() | inet:hostname(),
    name               :: server_name(),
    parent             :: pid(),
    pool_name          :: pool_name(),
    port               :: inet:port_number(),
    reconnect          :: boolean(),
    reconnect_time_max :: time(),
    reconnect_time_min :: time(),
    reconnect_time     :: time(),
    socket             :: undefined | inet:socket(),
    timer_ref          :: undefined | timer:ref()
}).

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

    ClientState = ?LOOKUP(state, Options),
    ConnectOptions = ?LOOKUP(connect_options, Options, ?DEFAULT_CONNECT_OPTS),
    Ip = ?LOOKUP(ip, Options, ?DEFAULT_IP),
    Port = ?LOOKUP(port, Options),
    Reconnect = ?LOOKUP(reconnect, Options, ?DEFAULT_RECONNECT),
    ReconnectTimeMax = ?LOOKUP(reconnect_time_max, Options, ?DEFAULT_RECONNECT_MAX),
    ReconnectTimeMin = ?LOOKUP(reconnect_time_min, Options, ?DEFAULT_RECONNECT_MIN),

    loop(#state {
        client = Client,
        client_state = ClientState,
        connect_options = ConnectOptions,
        ip = Ip,
        name = Name,
        parent = Parent,
        pool_name = PoolName,
        port = Port,
        reconnect = Reconnect,
        reconnect_time = ReconnectTimeMin,
        reconnect_time_max = ReconnectTimeMax,
        reconnect_time_min = ReconnectTimeMin
    }).

%% sys callbacks
-spec system_code_change(#state {}, module(), undefined | term(), term()) ->
    {ok, #state {}}.

system_code_change(State, _Module, _OldVsn, _Extra) ->
    {ok, State}.

-spec system_continue(pid(), [], #state {}) -> ok.

system_continue(_Parent, _Debug, State) ->
    loop(State).

-spec system_terminate(term(), pid(), [], #state {}) -> none().

system_terminate(Reason, _Parent, _Debug, _State) ->
    exit(Reason).

%% private
cancel_timer(undefined) ->
    ok;
cancel_timer(TimerRef) ->
    erlang:cancel_timer(TimerRef).

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
        client = Client,
        client_state = ClientState,
        connect_options = ConnectOptions,
        ip = Ip,
        pool_name = PoolName,
        port = Port,
        reconnect_time_min = TimeMin
    } = State) ->

    Options = [
        binary,
        {active, false},
        {packet, raw}
    ] ++ ConnectOptions,

    case gen_tcp:connect(Ip, Port, Options) of
        {ok, Socket} ->
            case Client:after_connect(Socket, ClientState) of
                {ok, ClientState2} ->
                    inet:setopts(Socket, [{active, true}]),

                    {ok, State#state {
                        client_state = ClientState2,
                        socket = Socket,
                        reconnect_time = TimeMin
                    }};
                {error, Reason, ClientState2} ->
                    shackle_utils:warning_msg(PoolName, "after connect error: ~p", [Reason]),

                    reconnect_time(State#state {
                        client_state = ClientState2
                    })
            end;
        {error, Reason} ->
            shackle_utils:warning_msg(PoolName, "tcp connect error: ~p", [Reason]),
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
        client = Client,
        client_state = ClientState,
        pool_name = PoolName,
        name = Name,
        socket = Socket
    } = State) ->

    {ok, ExtRequestId, Data, ClientState2} = Client:handle_request(Request, ClientState),

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
handle_msg({tcp, _Port, Data}, #state {
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
                shackle_utils:info_msg(PoolName, "shackle_queue not found: ~p", [ExtRequestId])
        end
    end, Replies),

    {ok, State#state {
        client_state = ClientState2
    }};
handle_msg({tcp_closed, Socket}, #state {
        socket = Socket,
        pool_name = PoolName
    } = State) ->

    shackle_utils:warning_msg(PoolName, "tcp connection closed", []),
    tcp_close(State);
handle_msg({tcp_error, Socket, Reason}, #state {
        socket = Socket,
        pool_name = PoolName
    } = State) ->

    shackle_utils:warning_msg(PoolName, "tcp connection error: ~p", [Reason]),
    gen_tcp:close(Socket),
    tcp_close(State).

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

    cancel_timer(TimerRef),
    ok = Client:terminate(ClientState),
    ok = shackle_backlog:delete(Name),
    reply_all(Name, {error, shutdown}),
    exit(Reason).
