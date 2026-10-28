-module(shackle_backlog).
-include("shackle_internal.hrl").

%% internal
-export([
    check/3,
    decrement/3,
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
    Ix = ix(Index),
    case atomics:add_get(Ref, Ix, 1) of
        Value when Value =< BacklogSize ->
            true;
        _Value ->
            atomics:sub(Ref, Ix, 1),
            false
    end.

-spec decrement(atomics:atomics_ref(), shackle_server:index(), backlog_size()) ->
    ok.

decrement(_Ref, _Index, infinity) ->
    ok;
decrement(Ref, Index, _BacklogSize) ->
    Ix = ix(Index),
    case atomics:sub_get(Ref, Ix, 1) of
        Value when Value < 0 ->
            %% a reset can occur while requests are in flight: do not
            %% go below zero
            atomics:add(Ref, Ix, 1);
        _Value ->
            ok
    end.

-spec new(shackle_pool:pool_size()) ->
    atomics:atomics_ref().

new(PoolSize) ->
    atomics:new(PoolSize * ?STRIDE, []).

-spec reset(atomics:atomics_ref(), shackle_server:index()) ->
    ok.

reset(Ref, Index) ->
    atomics:put(Ref, ix(Index), 0).

%% private
ix(Index) ->
    (Index - 1) * ?STRIDE + 1.
