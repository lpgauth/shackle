-module(shackle_utils).

-compile(inline).
-compile({inline_size, 512}).

%% public
-export([
    ets_options/0,
    lookup/3,
    random/1,
    random_element/1
]).

%% public
-spec ets_options() ->
  list().

ets_options() -> [
      named_table,
      public,
      {write_concurrency, true},
      {decentralized_counters, true}
  ].

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
    knot:uniform(N).

-spec random_element([term()]) ->
    term().

random_element([X]) ->
    X;
random_element([_|_] = List) ->
    T = list_to_tuple(List),
    element(random(tuple_size(T)), T).
