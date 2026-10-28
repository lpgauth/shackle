-module(shackle_backlog).

%% internal
-export([
    check/3,
    decrement/2,
    new/1,
    reset/2
]).

%% types
-type backlog_size() :: pos_integer() | infinity.

-export_type([
    backlog_size/0
]).

%% internal
-spec check(atomics:atomics_ref(), shackle_server:index(), backlog_size()) ->
    boolean().

check(_Ref, _Index, infinity) ->
    true;
check(Ref, Index, BacklogSize) ->
    case atomics:add_get(Ref, Index, 1) of
        Value when Value =< BacklogSize ->
            true;
        _Value ->
            atomics:sub(Ref, Index, 1),
            false
    end.

-spec decrement(atomics:atomics_ref(), shackle_server:index()) ->
    ok.

decrement(Ref, Index) ->
    case atomics:sub_get(Ref, Index, 1) of
        Value when Value < 0 ->
            %% reset or infinity backlog: never go below empty
            atomics:add(Ref, Index, 1);
        _Value ->
            ok
    end.

-spec new(shackle_pool:pool_size()) ->
    atomics:atomics_ref().

new(PoolSize) ->
    atomics:new(PoolSize, []).

-spec reset(atomics:atomics_ref(), shackle_server:index()) ->
    ok.

reset(Ref, Index) ->
    atomics:put(Ref, Index, 0).
