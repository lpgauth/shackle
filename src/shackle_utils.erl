-module(shackle_utils).
-include("shackle.hrl").

%% public
-export([
    child_name/2,
    child_names/2,
    info_msg/2,
    lookup/3,
    timeout/2,
    warning_msg/2
]).

%% public
-spec child_name(module(), pos_integer()) -> atom().

child_name(Module, N) ->
    list_to_atom(atom_to_list(Module) ++ integer_to_list(N)).

-spec child_names(module(), pos_integer()) -> [atom()].

child_names(Module, PoolSize) ->
    [shackle_utils:child_name(Module, N) || N <- lists:seq(1, PoolSize)].

-spec info_msg(string(), [term()]) -> ok.

info_msg(Format, Data) ->
    ?IF_DEF_TEST(fun () -> error_logger:info_msg(Format, Data) end).

-spec lookup(atom(), [{atom(), term()}], term()) -> term().

lookup(Key, List, Default) ->
    case lists:keyfind(Key, 1, List) of
        false -> Default;
        {_, Value} -> Value
    end.

-spec timeout(pos_integer(), erlang:timestamp()) -> integer().

timeout(Timeout, Timestamp) ->
    Diff = timer:now_diff(os:timestamp(), Timestamp) div 1000,
    Timeout - Diff.

-spec warning_msg(string(), [term()]) -> ok.

warning_msg(Format, Data) ->
    ?IF_DEF_TEST(fun () -> error_logger:warning_msg(Format, Data) end).
