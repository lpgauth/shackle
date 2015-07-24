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
    client           = undefined :: module(),
    client_state     = undefined :: term(),
    connect_options  = undefined :: [gen_tcp:connect_option()],
    ip               = undefined :: inet:ip_address() | inet:hostname(),
    name             = undefined :: atom(),
    parent           = undefined :: pid(),
    pool_name        = undefined :: atom(),
    port             = undefined :: inet:port_number(),
    reconnect        = true      :: boolean(),
    reconnect_max    = undefined :: non_neg_integer(),
    reconnect_min    = undefined :: non_neg_integer(),
    reconnect_time   = undefined :: non_neg_integer(),
    socket           = undefined :: undefined | inet:socket(),
    timer            = undefined :: undefined | timer:ref()
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
    {ok, Options} = Client:options(),

    ClientState = ?LOOKUP(state, Options),
    ConnectOptions = ?LOOKUP(connect_options, Options, ?DEFAULT_CONNECT_OPTS),
    Ip = ?LOOKUP(ip, Options, ?DEFAULT_IP),
    Port = ?LOOKUP(port, Options),
    Reconnect = ?LOOKUP(reconnect, Options, ?DEFAULT_RECONNECT),
    ReconnectMax = ?LOOKUP(reconnect_max, Options, ?DEFAULT_RECONNECT_MAX),
    ReconnectMin = ?LOOKUP(reconnect_min, Options, ?DEFAULT_RECONNECT_MIN),

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
        reconnect_max = ReconnectMax,
        reconnect_min = ReconnectMin,
        reconnect_time = ReconnectMin

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
reconnect_time(#state {
        reconnect_max = Max,
        reconnect_time = Time
    } = State) ->

    Time2 = shackle_backoff:timeout(Time, Max),

    {ok, State#state {
        reconnect_time = Time2,
        socket = undefined,
        timer = erlang:send_after(Time2, self(), ?MSG_CONNECT)
    }}.

handle_msg(?MSG_CONNECT, #state {
        client = Client,
        client_state = ClientState,
        connect_options = ConnectOptions,
        ip = Ip,
        pool_name = PoolName,
        port = Port,
        reconnect_min = ReconnectMin
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
                        reconnect_time = ReconnectMin
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
