-module(shackle_status).
-include("shackle_internal.hrl").

-compile(inline).
-compile({inline_size, 512}).

-export([
    active/1,
    delete/1,
    disable/1,
    enable/1,
    init/0,
    new/2
]).

%% public
-spec active(shackle_server:id()) ->
    boolean().

active({PoolName, ServerIndex}) ->
    case persistent_term:get(PoolName, undefined) of
        undefined ->
            false;
        Counters ->
            boolean(counters:get(Counters, ServerIndex))
    end.

-spec delete(shackle_pool:name()) ->
    ok.

delete(PoolName) ->
    persistent_term:erase(PoolName),
    ok.

-spec disable(shackle_server:id()) ->
    ok.

disable({PoolName, ServerIndex}) ->
    case persistent_term:get(PoolName, undefined) of
        undefined ->
            ok;
        Counters ->
            counters:put(Counters, ServerIndex, 0)
    end.

-spec enable(shackle_server:id()) ->
    ok.

enable({PoolName, ServerIndex}) ->
    case persistent_term:get(PoolName, undefined) of
        undefined ->
            ok;
        Counters ->
            counters:put(Counters, ServerIndex, 1)
    end.

-spec init() ->
    ok.

init() ->
    ok.

-spec new(shackle_pool:name(), shackle_pool:pool_size()) ->
    ok.

new(PoolName, PoolSize) ->
    Counters = counters:new(PoolSize, []),
    persistent_term:put(PoolName, Counters).

%% private
boolean(0) ->
    false;
boolean(1) ->
    true.
