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
    header             :: iodata(),
    ip                 :: inet:ip_address() | inet:hostname(),
    name               :: server_name(),
    parent             :: pid(),
    pool_name          :: pool_name(),
    port               :: inet:port_number(),
    protocol           :: protocol(),
    % TODO: refactor into shackle_backoff record
    reconnect          :: boolean(),
    reconnect_time_max :: time(),
    reconnect_time_min :: time(),
    reconnect_time     :: time(),
    socket             :: undefined | inet:socket(),
    socket_options     :: [gen_tcp:connect_option() | gen_udp:connect_option()],
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

    {ok, Addrs} = inet:getaddrs(Ip, inet),
    Ip2 = shackle_utils:random_element(Addrs),
    Header = shackle_udp:header(Ip2, Port),

    loop(#state {
        client = Client,
        header = Header,
        ip = Ip2,
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
close(#state {name = Name} = State) ->
    reply_all(Name, {error, socket_closed}),
    reconnect(State).

handle_msg(?MSG_CONNECT, #state {
        client = Client,
        ip = Ip,
        pool_name = PoolName,
        port = Port,
        protocol = Protocol,
        reconnect_time_min = TimeMin,
        socket_options = SocketOptions
    } = State) ->

    case Protocol:new(Ip, Port, SocketOptions) of
        {ok, Socket} ->
            {ok, ClientState} = Client:init(),
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

                    reconnect(State#state {
                        client_state = ClientState2
                    })
            end;
        {error, Reason} ->
            shackle_utils:warning_msg(PoolName,
                "~p connect error: ~p", [Protocol, Reason]),
            reconnect(State)
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
        header = Header,
        pool_name = PoolName,
        protocol = Protocol,
        name = Name,
        socket = Socket
    } = State) ->

    {ok, ExtRequestId, Data, ClientState2} =
        Client:handle_request(Request, ClientState),

    case Protocol:send(Socket, Header, Data) of
        ok ->
            shackle_queue:in(Name, ExtRequestId, Cast#cast {
                timing = shackle_utils:timing(Timestamp, Timing)
            }),

            {ok, State#state {
                client_state = ClientState2
            }};
        {error, Reason} ->
            shackle_utils:warning_msg(PoolName, "tcp send error: ~p", [Reason]),
            Protocol:close(Socket),
            close(State)
    end;
handle_msg({inet_reply, _Socket, ok}, State) ->
    {ok, State};
handle_msg({tcp, _Port, Data}, State) ->
    handle_msg_data(Data, State);
handle_msg({tcp_closed, Socket}, #state {
        socket = Socket,
        pool_name = PoolName
    } = State) ->

    shackle_utils:warning_msg(PoolName, "tcp connection closed", []),
    close(State);
handle_msg({tcp_error, Socket, Reason}, #state {
        socket = Socket,
        pool_name = PoolName
    } = State) ->

    shackle_utils:warning_msg(PoolName, "tcp connection error: ~p", [Reason]),
    shackle_tcp:close(Socket),
    close(State);
handle_msg({udp, _Socket, _Ip, _InPortNo, Data}, State) ->
    handle_msg_data(Data, State).

handle_msg_data(Data, #state {
        client = Client,
        client_state = ClientState
    } = State) ->

    {ok, Replies, ClientState2} = Client:handle_data(Data, ClientState),
    ok = process_replies(Replies, State),

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

process_replies([], _State) ->
    ok;
process_replies([{ExtRequestId, Reply} | T], #state {name = Name} = State) ->
    case shackle_queue:out(Name, ExtRequestId) of
        {ok, #cast {
            timestamp = Timestamp,
            timing = Timing
        } = Cast} ->

            reply(Name, Reply, Cast#cast {
                timing = shackle_utils:timing(Timestamp, Timing)
            });
        {error, not_found} ->
            ok
    end,
    process_replies(T, State).

reconnect(#state {client_state = undefined} = State) ->
    reconnect_timer(State);
reconnect(#state {
        client = Client,
        client_state = ClientState
    } = State) ->

    ok = Client:terminate(ClientState),
    reconnect_timer(State).

reconnect_timer(#state {reconnect = false} = State) ->
    {ok, State#state {
        socket = undefined
    }};
reconnect_timer(#state {
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

reply(Name, Reply, #cast {pid = Pid} = Cast) ->
    shackle_backlog:decrement(Name),
    Pid ! Cast#cast {
        reply = Reply
    }.

reply_all(Name, Reply) ->
    Requests = shackle_queue:all(Name),
    [reply(Name, Reply, Request) || Request <- Requests].

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
