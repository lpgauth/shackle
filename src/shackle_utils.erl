-module(shackle_utils).
-include("shackle_internal.hrl").

-compile(inline).
-compile({inline_size, 512}).

%% public
-export([
    cancel_timer/1,
    client_setup/3,
    lookup/3,
    process_responses/2,
    random/1,
    random_element/1,
    warning_msg/3,
    reconnect_state/1,
    reconnect_state_reset/1,
    reply/3,
    reply_all/2
]).

%% public
-spec cancel_timer(undefined | reference()) ->
    ok.

cancel_timer(undefined) ->
    ok;
cancel_timer(TimerRef) ->
    erlang:cancel_timer(TimerRef).

-spec client_setup(client(), pool_name(), inet:socket()) ->
    {ok, client_state()} | {error, term(), client_state()}.

client_setup(Client, PoolName, Socket) ->
    {ok, ClientState} = Client:init(),
    inet:setopts(Socket, [{active, false}]),
    case Client:setup(Socket, ClientState) of
        {ok, ClientState2} ->
            inet:setopts(Socket, [{active, true}]),
            {ok, ClientState2};
        {error, Reason, ClientState2} ->
            shackle_utils:warning_msg(PoolName,
                "setup error: ~p", [Reason]),
            {error, Reason, ClientState2}
    end.

-spec lookup(atom(), [{atom(), term()}], term()) ->
    term().

lookup(Key, List, Default) ->
    case lists:keyfind(Key, 1, List) of
        false -> Default;
        {_, Value} -> Value
    end.

-spec process_responses([response()], server_name()) ->
    ok.

process_responses([], _Name) ->
    ok;
process_responses([{ExtRequestId, Reply} | T], Name) ->
    case shackle_queue:remove(Name, ExtRequestId) of
        {ok, Cast, TimerRef} ->
            erlang:cancel_timer(TimerRef),
            reply(Name, Reply, Cast);
        {error, not_found} ->
            ok
    end,
    process_responses(T, Name).

-spec random(pos_integer()) ->
    non_neg_integer().

random(1) -> 1;
random(N) ->
    granderl:uniform(N).

-spec random_element([term()]) ->
    term().

random_element([X]) ->
    X;
random_element([_|_] = List) ->
    T = list_to_tuple(List),
    element(random(tuple_size(T)), T).

-spec reconnect_state(client_options()) ->
    undefined | reconnect_state().

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

-spec reconnect_state_reset(undefined | reconnect_state()) ->
    undefined | reconnect_state().

reconnect_state_reset(undefined) ->
    undefined;
reconnect_state_reset(#reconnect_state {} = ReconnectState) ->
    ReconnectState#reconnect_state {
        current = undefined
    }.

-spec reply(server_name(), term(), undefined | cast()) ->
    ok.

reply(Name, _Reply, #cast {pid = undefined}) ->
    shackle_backlog:decrement(Name),
    ok;
reply(Name, Reply, #cast {pid = Pid} = Cast) ->
    shackle_backlog:decrement(Name),
    Pid ! {Cast, Reply},
    ok.

-spec reply_all(server_name(), term()) ->
    ok.

reply_all(Name, Reply) ->
    reply_all(Name, Reply, shackle_queue:clear(Name)).

-spec warning_msg(pool_name(), string(), [term()]) ->
    ok.

warning_msg(Pool, Format, Data) ->
    error_logger:warning_msg("[~p] " ++ Format, [Pool | Data]).

%% private
reply_all(_Name, _Reply, []) ->
    ok;
reply_all(Name, Reply, [{Cast, TimerRef} | T]) ->
    erlang:cancel_timer(TimerRef),
    reply(Name, Reply, Cast),
    reply_all(Name, Reply, T).
