-module(shackle_server).
-include("shackle.hrl").

-export([
    init/2,
    start_link/1
]).

%% callbacks
-callback init() -> ok.
-callback after_connect() -> ok.
-callback handle_cast() -> ok.
-callback handle_data() -> ok.
-callback terminate() -> ok.

-record(state, {
    connect_retry = 0,
    ip            = undefined,
    port          = undefined,
    reconnect     = undefined,
    socket        = undefined,
    timer         = undefined
}).

-define(MSG_CONNECT, connect).

-spec init(pid(), atom()) -> no_return().

init(Parent, Name) ->
    register(Name, self()),
    proc_lib:init_ack(Parent, {ok, self()}),

    shackle_backlog:new(Name),
    self() ! ?MSG_CONNECT,

    loop(#state {}).

-spec start_link(atom()) -> {ok, pid()}.

start_link(Name) ->
    proc_lib:start_link(?MODULE, init, [self(), Name]).

loop(State) ->
    receive Msg ->
        {ok, State2} = handle_msg(Msg, State),
        loop(State2)
    end.

handle_msg(_Msg, State) ->
    {ok, State}.
