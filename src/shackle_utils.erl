-module(shackle_utils).
-include("shackle_internal.hrl").

-compile(inline).
-compile({inline_size, 512}).

%% public
-export([
    lookup/3,
    random/1,
    random_element/1,
    warning_msg/3
]).

%% public
-spec lookup(atom(), [{atom(), term()}], term()) ->
    term().

lookup(Key, List, Default) ->
    case lists:keyfind(Key, 1, List) of
        false -> Default;
        {_, Value} -> Value
    end.

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

-spec warning_msg(pool_name(), string(), [term()]) ->
    ok.

warning_msg(Pool, Format, Data) ->
    error_logger:warning_msg("[~p] " ++ Format, [Pool | Data]).
