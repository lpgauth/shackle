-module(shackle_manager).
-include("shackle_internal.hrl").

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

-record(state, {
    client :: client(),
    parent :: pid()
}).

-type state() :: #state {}.

%% public
-spec start_link(server_name(), client()) -> {ok, pid()}.

start_link(Name, Client) ->
    proc_lib:start_link(?MODULE, init, [Name, Client, self()]).

-spec init(server_name(), client(), pid()) -> no_return().

init(Name, Client, Parent) ->
    process_flag(trap_exit, true),
    proc_lib:init_ack(Parent, {ok, self()}),
    register(Name, self()),

    loop(#state {
        client = Client,
        parent = Parent
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
handle_msg({timeout, {ServerName, _} = RequestId, Pid},
        #state {client = Client} = State) ->

    shackle_backlog:decrement(ServerName),
    case shackle_queue:remove(RequestId) of
        {ok, Cast} ->
            Pid ! Cast#cast {
                reply = {error, timeout}
            };
        {error, not_found} ->
            Pid ! #cast {
                client = Client,
                pid = Pid,
                reply = {error, timeout},
                request_id = RequestId
            }
    end,
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

terminate(Reason, _State) ->
    exit(Reason).
