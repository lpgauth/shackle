-module(shackle_server).
-include("shackle.hrl").

%% internal
-export([
    init/3,
    start_link/2
]).

%% sys behavior
-export([
    system_code_change/4,
    system_continue/3,
    system_terminate/4
]).

%% callbacks
-callback init() -> {ok, init_opts()}.
-callback after_connect(Socket :: inet:socket(), State :: term()) -> {ok, Socket :: inet:socket(), State :: term()}.
-callback handle_cast(Request :: term(), State :: term()) -> {ok, RequestId :: term(), Data :: binary(), State :: term()}.
-callback handle_data(Data :: binary(), State :: term()) -> {ok, [{RequestId :: term(), Reply :: term()}], State :: term()}.
-callback terminate(State :: term()) -> ok.

-record(state, {
    connect_retry = 0         :: non_neg_integer(),
    ip            = undefined :: inet:ip_address() | inet:hostname(),
    module        = undefined :: module(),
    name          = undefined :: atom(),
    parent        = undefined :: pid(),
    port          = undefined :: inet:port_number(),
    reconnect     = true      :: boolean(),
    socket        = undefined :: undefined | inet:socket(),
    state         = undefined :: term(),
    timer         = undefined :: undefined | timer:ref()
}).

%% public
-spec start_link(atom(), module()) -> {ok, pid()}.

start_link(Name, Module) ->
    proc_lib:start_link(?MODULE, init, [Name, Module, self()]).

-spec init(atom(), module(), pid()) -> no_return().

init(Name, Module, Parent) ->
    process_flag(trap_exit, true),
    proc_lib:init_ack(Parent, {ok, self()}),
    register(Name, self()),

    self() ! ?MSG_CONNECT,
    shackle_backlog:new(Name),
    {ok, Opts} = Module:init(),

    loop(#state {
        ip = ?LOOKUP(ip, Opts),
        module = Module,
        name = Name,
        parent = Parent,
        port = ?LOOKUP(port, Opts),
        reconnect = ?LOOKUP(reconnect, Opts),
        state = ?LOOKUP(state, Opts)
    }).

%% sys callbacks
-spec system_code_change(#state {}, module(), undefined | term(), term()) -> {ok, #state {}}.

system_code_change(State, _Module, _OldVsn, _Extra) ->
    {ok, State}.

-spec system_continue(pid(), [], #state {}) -> ok.

system_continue(_Parent, _Debug, State) ->
    loop(State).

-spec system_terminate(term(), pid(), [], #state {}) -> none().

system_terminate(Reason, _Parent, _Debug, _State) ->
    exit(Reason).

%% private
connect_retry(#state {reconnect = false} = State) ->
    {ok, State#state {
        socket = undefined
    }};
connect_retry(#state {connect_retry = ConnectRetry} = State) ->
    Timeout = shackle_backoff:timeout(ConnectRetry),

    {ok, State#state {
        connect_retry = ConnectRetry + 1,
        socket = undefined,
        timer = erlang:send_after(Timeout, self(), ?MSG_CONNECT)
    }}.

-spec handle_msg(term(), #state {}) -> {ok, #state {}}.

handle_msg(?MSG_CONNECT, #state {
        ip = Ip,
        port = Port
    } = State) ->

    Opts = [
        binary,
        {active, true},
        {packet, raw},
        {send_timeout, ?DEFAULT_SEND_TIMEOUT},
        {send_timeout_close, true}
    ],

    case gen_tcp:connect(Ip, Port, Opts) of
        {ok, Socket} ->
            {ok, State#state {
                socket = Socket,
                connect_retry = 0
            }};
        {error, Reason} ->
            shackle_utils:warning_msg("tcp connect error: ~p", [Reason]),
            connect_retry(State)
    end;
handle_msg({call, Ref, From, _Msg}, #state {
        socket = undefined,
        module = Module,
        name = Name
    } = State) ->

    reply(Module, Name, Ref, From, {error, no_socket}),
    {ok, State};
handle_msg({call, Ref, From, Request}, #state {
        module = Module,
        name = Name,
        socket = Socket,
        state = ClientState
    } = State) ->

    {ok, RequestId, Data, ClientState2} = Module:handle_cast(Request, ClientState),

    case gen_tcp:send(Socket, Data) of
        ok ->
            shackle_queue:in(Name, RequestId, {Ref, From}),
            {ok, State#state {
                state = ClientState2
            }};
        {error, Reason} ->
            shackle_utils:warning_msg("tcp send error: ~p", [Reason]),
            gen_tcp:close(Socket),
            tcp_close(State)
    end;
handle_msg({tcp, _Port, Data}, #state {
        module = Module,
        name = Name,
        state = ClientState
    } = State) ->

    {ok, Replys, ClientState2} = Module:handle_data(Data, ClientState),

    lists:foreach(fun ({RequestId, Reply}) ->
        {Ref, From} = shackle_queue:out(Name, RequestId),
        reply(Module, Name, Ref, From, Reply)
    end, Replys),

    {ok, State#state {
        state = ClientState2
    }};
handle_msg({tcp_closed, Socket}, #state {
        socket = Socket
    } = State) ->

    shackle_utils:warning_msg("tcp closed", []),
    tcp_close(State);
handle_msg({tcp_error, Socket, Reason}, #state {
        socket = Socket
    } = State) ->

    shackle_utils:warning_msg("tcp error: ~p", [Reason]),
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

reply(Module, Name, Ref, From, Msg) ->
    shackle_backlog:decrement(Name),
    % TODO: fix me
    From ! {Module, Ref, Msg}.

tcp_close(#state {module = Module, name = Name} = State) ->
    Msg = {error, tcp_closed},
    Items = shackle_queue:all(Name),
    [reply(Module, Name, Ref, From, Msg) || {Ref, From} <- Items],
    connect_retry(State).

terminate(_Reason, _State) -> ok.
