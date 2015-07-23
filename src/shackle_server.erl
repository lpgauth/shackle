-module(shackle_server).
-include("shackle.hrl").

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
    client         = undefined :: module(),
    client_state   = undefined :: term(),
    ip             = undefined :: inet:ip_address() | inet:hostname(),
    name           = undefined :: atom(),
    parent         = undefined :: pid(),
    pool_name      = undefined :: atom(),
    port           = undefined :: inet:port_number(),
    reconnect      = true      :: boolean(),
    reconnect_time = undefined :: non_neg_integer(),
    socket         = undefined :: undefined | inet:socket(),
    timer          = undefined :: undefined | timer:ref()
}).

%% public
-spec start_link(atom(), atom(), module()) -> {ok, pid()}.

start_link(Name, PoolName, Client) ->
    proc_lib:start_link(?MODULE, init, [Name, PoolName, Client, self()]).

-spec init(atom(), atom(), module(), pid()) -> no_return().

init(Name, PoolName, Client, Parent) ->
    process_flag(trap_exit, true),
    proc_lib:init_ack(Parent, {ok, self()}),
    register(Name, self()),

    random:seed(os:timestamp()),
    self() ! ?MSG_CONNECT,
    ok = shackle_backlog:new(Name),
    {ok, Opts} = Client:init(),

    loop(#state {
        client = Client,
        client_state = ?LOOKUP(state, Opts),
        ip = ?LOOKUP(ip, Opts, ?DEFAULT_IP),
        name = Name,
        parent = Parent,
        pool_name = PoolName,
        port = ?LOOKUP(port, Opts),
        reconnect = ?LOOKUP(reconnect, Opts, ?DEFAULT_RECONNECT),
        reconnect_time = ?LOOKUP(reconnect_time, Opts, ?DEFAULT_RECONNECT_TIME)
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
reconnect_time(#state {reconnect = false} = State) ->
    {ok, State#state {
        socket = undefined
    }};
reconnect_time(#state {reconnect_time = ReconnectTime} = State) ->
    ReconnectTime2 = shackle_backoff:timeout(ReconnectTime),

    {ok, State#state {
        reconnect_time = ReconnectTime2,
        socket = undefined,
        timer = erlang:send_after(ReconnectTime2, self(), ?MSG_CONNECT)
    }}.

handle_msg(?MSG_CONNECT, #state {
        client = Client,
        client_state = ClientState,
        ip = Ip,
        pool_name = PoolName,
        port = Port
    } = State) ->

    Opts = [
        binary,
        {active, false},
        {packet, raw},
        {send_timeout, ?DEFAULT_SEND_TIMEOUT},
        {send_timeout_close, true}
    ],

    case gen_tcp:connect(Ip, Port, Opts) of
        {ok, Socket} ->
            case Client:after_connect(Socket, ClientState) of
                {ok, ClientState2} ->
                    inet:setopts(Socket, [{active, true}]),

                    {ok, State#state {
                        client_state = ClientState2,
                        socket = Socket,
                        reconnect_time = ?DEFAULT_RECONNECT_TIME
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
handle_msg({call, Ref, From, _Msg}, #state {
        socket = undefined
    } = State) ->

    reply(Ref, From, {error, no_socket}, State),
    {ok, State};
handle_msg({call, Ref, From, Request}, #state {
        client = Client,
        client_state = ClientState,
        pool_name = PoolName,
        name = Name,
        socket = Socket
    } = State) ->

    {ok, RequestId, Data, ClientState2} = Client:handle_cast(Request, ClientState),

    case gen_tcp:send(Socket, Data) of
        ok ->
            shackle_queue:in(Name, RequestId, {Ref, From}),

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

    lists:foreach(fun ({RequestId, Reply}) ->
        case shackle_queue:out(Name, RequestId) of
            {ok, {Ref, From}} ->
                reply(Ref, From, Reply, State);
            {error, not_found} ->
                shackle_utils:info_msg(PoolName, "shackle_queue not found: ~p", [RequestId])
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

reply(Ref, From, Msg, #state {
        name = Name,
        pool_name = PoolName
    }) ->

    shackle_backlog:decrement(Name),
    From ! {PoolName, Ref, Msg}.

reply_all(Name, Msg, State) ->
    Items = shackle_queue:all(Name),
    [reply(Ref, From, Msg, State) || {Ref, From} <- Items].

tcp_close(#state {name = Name} = State) ->
    reply_all(Name, {error, tcp_closed}, State),
    reconnect_time(State).

terminate(Reason, #state {
        client = Client,
        client_state = ClientState,
        name = Name
    } = State) ->

    ok = Client:terminate(ClientState),
    ok = shackle_backlog:delete(Name),
    reply_all(Name, {error, shutdown}, State),
    exit(Reason).
