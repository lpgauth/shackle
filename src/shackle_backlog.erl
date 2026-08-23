-module(shackle_backlog).

%% internal
-export([
    check/3,
    decrement/2,
    delete/1,
    new/2,
    ref/1,
    reset/2
]).

%% types
-type backlog_size() :: pos_integer() | infinity.

-export_type([
    backlog_size/0
]).

%% internal
-spec check(atomics:atomics_ref(), shackle_server:id(), backlog_size()) ->
    boolean().

check(_Ref, _ServerId, infinity) ->
    true;
check(Ref, {_PoolName, Index}, BacklogSize) ->
    case atomics:add_get(Ref, Index, 1) of
        Value when Value =< BacklogSize ->
            true;
        _Value ->
            atomics:sub(Ref, Index, 1),
            false
    end.

-spec decrement(atomics:atomics_ref(), shackle_server:id()) ->
    integer().

decrement(Ref, {_PoolName, Index}) ->
    case atomics:sub_get(Ref, Index, 1) of
        Value when Value < 0 ->
            %% reset or infinity backlog: never go below empty
            atomics:add(Ref, Index, 1),
            0;
        Value ->
            Value
    end.

-spec delete(shackle_pool:name()) ->
    ok.

delete(PoolName) ->
    persistent_term:erase({?MODULE, PoolName}),
    ok.

-spec new(shackle_pool:name(), shackle_pool:pool_size()) ->
    ok.

new(PoolName, PoolSize) ->
    Ref = atomics:new(PoolSize, []),
    persistent_term:put({?MODULE, PoolName}, Ref),
    ok.

-spec ref(shackle_pool:name()) ->
    atomics:atomics_ref().

ref(PoolName) ->
    persistent_term:get({?MODULE, PoolName}).

-spec reset(shackle_pool:name(), shackle_server:id()) ->
    ok.

reset(PoolName, {_PoolName, Index}) ->
    case persistent_term:get({?MODULE, PoolName}, undefined) of
        undefined ->
            ok;
        Ref ->
            atomics:put(Ref, Index, 0)
    end.
