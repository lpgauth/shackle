-module(shackle_utils).
-include("shackle.hrl").

-export([
    child_name/2,
    child_specs/2,
    lookup/3,
    timeout/2
]).

%% public
child_name(Mod, N) ->
    list_to_atom(atom_to_list(Mod) ++ integer_to_list(N)).

child_specs(Module, PoolSize) ->
    [?CHILD(shackle_utils:child_name(Module, N), Module) || N <- lists:seq(1, PoolSize)].

lookup(Key, List, Default) ->
    case lists:keyfind(Key, 1, List) of
        false -> Default;
        {_, Value} -> Value
    end.

timeout(Timeout, Timestamp) ->
    Diff = timer:now_diff(os:timestamp(), Timestamp) div 1000,
    Timeout - Diff.
