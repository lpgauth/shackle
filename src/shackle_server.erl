-module(shackle_server).
-include("shackle.hrl").

-export([
    init/3,
    start_link/2
]).

-export([
    system_code_change/4,
    system_continue/3,
    system_terminate/4
]).

-type init_opt() :: {ip, inet:ip_address() | inet:hostname()} |
                    {port, inet:port_number()} |
                    {reconnect, boolean()} |
                    {state, term()}.

-type init_opts() :: [init_opt()].

%% callbacks
-callback init() -> {ok, init_opts()}.
-callback after_connect(Socket :: inet:socket(), State :: term()) -> {ok, Socket :: inet:socket(), State :: term()}.
-callback handle_cast(Request :: term(), State :: term()) -> {ok, Data :: binary(), State :: term()}.
-callback handle_data(Data :: binary(), State :: term()) -> {ok, Reply :: term(), State :: term()}.
-callback terminate(State :: term()) -> ok.

-record(state, {
    connect_retry = 0         :: non_neg_integer(),
    ip            = undefined :: inet:ip_address() | inet:hostname(),
    parent        = undefined :: pid(),
    port          = undefined :: inet:port_number(),
    reconnect     = true      :: boolean(),
    socket        = undefined :: undefined | inet:socket(),
    state         = undefined :: term(),
    timer         = undefined :: undefined | timer:ref()
}).

-type state() :: #state {}.

-define(MSG_CONNECT, connect).

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
        parent = Parent,
        port = ?LOOKUP(port, Opts),
        reconnect = ?LOOKUP(reconnect, Opts),
        state = ?LOOKUP(state, Opts)
    }).

%% private
-spec handle_msg(term(), state()) -> {ok, state()}.
handle_msg(_Msg, State) ->
    {ok, State}.

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

system_code_change(State, _Moduleule, _OldVsn, _Extra) ->
    {ok, State}.

system_continue(_Parent, _Debug, State) ->
    loop(State).

system_terminate(Reason, _Parent, _Debug, _State) ->
    exit(Reason).

terminate(_Reason, _State) -> ok.
