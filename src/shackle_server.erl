-module(shackle_server).
-include("shackle_internal.hrl").

-compile(inline).
-compile({inline_size, 512}).

%% internal
-export([
    init/5,
    start_link/4
]).

%% sys behavior
-export([
    system_code_change/4,
    system_continue/3,
    system_get_state/1,
    system_terminate/4
]).

-record(state, {
    client           :: client(),
    header           :: undefined | iodata(),
    ip               :: inet:ip_address() | inet:hostname(),
    name             :: server_name(),
    parent           :: pid(),
    pool_name        :: pool_name(),
    port             :: inet:port_number(),
    protocol         :: protocol(),
    reconnect_state  :: undefined | reconnect_state(),
    socket           :: undefined | inet:socket(),
    socket_options   :: [gen_tcp:connect_option() | gen_udp:option()],
    timer_ref        :: undefined | reference()
}).

-type client_state() :: term().
-type state() :: #state {}.

%% public
-spec start_link(server_name(), pool_name(), client(), client_options()) ->
    {ok, pid()}.

start_link(Name, PoolName, Client, ClientOptions) ->
    proc_lib:start_link(?MODULE, init, [Name, PoolName, Client,
        ClientOptions, self()]).

-spec init(server_name(), pool_name(), client(), client_options(), pid()) ->
    no_return().

init(Name, PoolName, Client, ClientOptions, Parent) ->
    process_flag(trap_exit, true),
    proc_lib:init_ack(Parent, {ok, self()}),
    register(Name, self()),

    self() ! ?MSG_CONNECT,
    ok = shackle_backlog:new(Name),

    Ip = ?LOOKUP(ip, ClientOptions, ?DEFAULT_IP),
    Port = ?LOOKUP(port, ClientOptions),
    Protocol = ?LOOKUP(protocol, ClientOptions, ?DEFAULT_PROTOCOL),
    ReconnectState = reconnect_state(ClientOptions),
    SocketOptions = ?LOOKUP(socket_options, ClientOptions,
        ?DEFAULT_SOCKET_OPTS),

    loop(#state {
        client = Client,
        ip = Ip,
        name = Name,
        parent = Parent,
        pool_name = PoolName,
        port = Port,
        protocol = Protocol,
        reconnect_state = ReconnectState,
        socket_options = SocketOptions
    }, undefined).

%% sys callbacks
-spec system_code_change(state(), module(), undefined | term(), term()) ->
    {ok, state()}.

system_code_change(State, _Module, _OldVsn, _Extra) ->
    {ok, State}.

-spec system_continue(pid(), [], {state(), client_state()}) ->
    ok.

system_continue(_Parent, _Debug, {State, ClientState}) ->
    loop(State, ClientState).

-spec system_get_state(state()) ->
    {ok, state()}.

system_get_state(State) ->
    {ok, State}.

-spec system_terminate(term(), pid(), [], state()) ->
    none().

system_terminate(Reason, _Parent, _Debug, _State) ->
    exit(Reason).

%% private
close(#state {name = Name} = State, ClientState) ->
    reply_all(Name, {error, socket_closed}),
    reconnect(State, ClientState).

handle_msg(?MSG_CONNECT, #state {
        client = Client,
        ip = Ip,
        pool_name = PoolName,
        port = Port,
        protocol = Protocol,
        reconnect_state = ReconnectState,
        socket_options = SocketOptions
    } = State, ClientState) ->

    case inet:getaddrs(Ip, inet) of
        {ok, Addrs} ->
            Ip2 = shackle_utils:random_element(Addrs),
            Headers = shackle_udp:header(Ip2, Port),
            case Protocol:new(Ip2, Port, SocketOptions) of
                {ok, Socket} ->
                    {ok, ClientState2} = Client:init(),
                    inet:setopts(Socket, [{active, false}]),

                    case Client:setup(Socket, ClientState2) of
                        {ok, ClientState3} ->
                            inet:setopts(Socket, [{active, true}]),

                            {ok, State#state {
                                header = Headers,
                                reconnect_state =
                                    reconnect_state_reset(ReconnectState),
                                socket = Socket
                            }, ClientState3};
                        {error, Reason, ClientState3} ->
                            shackle_utils:warning_msg(PoolName,
                                "setup error: ~p", [Reason]),
                            Protocol:close(Socket),
                            reconnect(State, ClientState3)
                    end;
                {error, Reason} ->
                    shackle_utils:warning_msg(PoolName,
                        "~p connect error: ~p", [Protocol, Reason]),
                    reconnect(State, ClientState)
            end;

        {error, Reason} ->
            shackle_utils:warning_msg(PoolName,
                "~p getaddrs error: ~p", [Protocol, Reason]),
            reconnect(State, ClientState)
    end;
handle_msg(#cast {} = Cast, #state {
        socket = undefined,
        name = Name
    } = State, ClientState) ->

    reply(Name, {error, no_socket}, Cast),
    {ok, State, ClientState};
handle_msg(#cast {
        request = Request,
        timeout = Timeout
    } = Cast, #state {
        client = Client,
        header = Header,
        pool_name = PoolName,
        protocol = Protocol,
        socket = Socket
    } = State, ClientState) ->

    {ok, ExtRequestId, Data, ClientState2} =
        Client:handle_request(Request, ClientState),

    case Protocol:send(Socket, Header, Data) of
        ok ->
            TimerRef = erlang:send_after(Timeout, self(),
                {timeout, ExtRequestId}),
            shackle_queue:add(ExtRequestId, Cast, TimerRef),
            {ok, State, ClientState2};
        {error, Reason} ->
            shackle_utils:warning_msg(PoolName, "tcp send error: ~p", [Reason]),
            Protocol:close(Socket),
            close(State, ClientState2)
    end;
handle_msg({inet_reply, _Socket, ok}, State, ClientState) ->
    {ok, State, ClientState};
handle_msg({inet_reply, _Socket, {error, Reason}}, #state {
        pool_name = PoolName
    } = State, ClientState) ->

    shackle_utils:warning_msg(PoolName, "udp send error: ~p", [Reason]),
    {ok, State, ClientState};
handle_msg({tcp, _Port, Data}, State, ClientState) ->
    handle_msg_data(Data, State, ClientState);
handle_msg({tcp_closed, Socket}, #state {
        socket = Socket,
        pool_name = PoolName
    } = State, ClientState) ->

    shackle_utils:warning_msg(PoolName, "tcp connection closed", []),
    close(State, ClientState);
handle_msg({tcp_error, Socket, Reason}, #state {
        socket = Socket,
        pool_name = PoolName
    } = State, ClientState) ->

    shackle_utils:warning_msg(PoolName, "tcp connection error: ~p", [Reason]),
    shackle_tcp:close(Socket),
    close(State, ClientState);
handle_msg({timeout, ExtRequestId}, #state {
        name = Name
    } = State, ClientState) ->

    case shackle_queue:remove(Name, ExtRequestId) of
        {ok, Cast, _TimerRef} ->
            reply(Name, {error, timeout}, Cast);
        {error, not_found} ->
            ok
    end,
    {ok, State, ClientState};
handle_msg({udp, _Socket, _Ip, _InPortNo, Data}, State, ClientState) ->
    handle_msg_data(Data, State, ClientState).

handle_msg_data(Data, #state {client = Client} = State, ClientState) ->
    {ok, Replies, ClientState2} = Client:handle_data(Data, ClientState),
    ok = process_replies(Replies, State),
    {ok, State, ClientState2}.

loop(#state {parent = Parent} = State, ClientState) ->
    receive
        {'EXIT', Parent, Reason} ->
            terminate(Reason, State, ClientState);
        {system, From, Request} ->
            sys:handle_system_msg(Request, From, Parent, ?MODULE, [],
                {State, ClientState});
        Msg ->
            {ok, State2, ClientState2} = handle_msg(Msg, State, ClientState),
            loop(State2, ClientState2)
    end.

process_replies([], _State) ->
    ok;
process_replies([{ExtRequestId, Reply} | T], #state {name = Name} = State) ->
    case shackle_queue:remove(Name, ExtRequestId) of
        {ok, Cast, TimerRef} ->
            erlang:cancel_timer(TimerRef),
            reply(Name, Reply, Cast);
        {error, not_found} ->
            ok
    end,
    process_replies(T, State).

reconnect(State, undefined) ->
    reconnect_timer(State, undefined);
reconnect(#state {client = Client} = State, ClientState) ->
    ok = Client:terminate(ClientState),
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

    {ok, State#state {
        socket = undefined
    }, ClientState};
reconnect_timer(#state {
        reconnect_state = #reconnect_state {
            current = undefined,
            min = none
        } = ReconnectState
    } = State, ClientState) ->

    {ok, State2, ClientState2} = handle_msg(?MSG_CONNECT, State#state {
        reconnect_state = ReconnectState#reconnect_state {
            current = ?DEFAULT_RECONNECT_MIN
        },
        socket = undefined
    }, ClientState),

    {ok, State2, ClientState2};
reconnect_timer(#state {
        reconnect_state = ReconnectState
    } = State, ClientState)  ->

    #reconnect_state {
        current = Current
    } = ReconnectState2 = shackle_backoff:timeout(ReconnectState),

    TimerRef = erlang:send_after(Current, self(), ?MSG_CONNECT),

    {ok, State#state {
        reconnect_state = ReconnectState2,
        socket = undefined,
        timer_ref = TimerRef
    }, ClientState}.

reply(Name, _Reply, #cast {pid = undefined}) ->
    shackle_backlog:decrement(Name);
reply(Name, Reply, #cast {pid = Pid} = Cast) ->
    shackle_backlog:decrement(Name),
    Pid ! {Cast, Reply}.

reply_all(Name, Reply) ->
    Requests = shackle_queue:clear(Name),
    [reply(Name, Reply, Request) || Request <- Requests].

terminate(Reason, #state {
        client = Client,
        name = Name,
        timer_ref = TimerRef
    }, ClientState) ->

    shackle_utils:cancel_timer(TimerRef),
    ok = Client:terminate(ClientState),
    reply_all(Name, {error, shutdown}),
    ok = shackle_backlog:delete(Name),
    exit(Reason).
