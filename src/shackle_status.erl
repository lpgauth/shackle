-module(shackle_status).

-export([
    active/2,
    disable/2,
    enable/2,
    new/1
]).

%% public
-spec active(counters:counters_ref(), shackle_server:index()) ->
    boolean().

active(Ref, Index) ->
    counters:get(Ref, Index) =:= 1.

-spec disable(counters:counters_ref(), shackle_server:index()) ->
    ok.

disable(Ref, Index) ->
    counters:put(Ref, Index, 0).

-spec enable(counters:counters_ref(), shackle_server:index()) ->
    ok.

enable(Ref, Index) ->
    counters:put(Ref, Index, 1).

-spec new(shackle_pool:pool_size()) ->
    counters:counters_ref().

new(PoolSize) ->
    counters:new(PoolSize, []).
