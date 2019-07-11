-module(shackle_status).
-include("shackle_internal.hrl").

-compile(inline).
-compile({inline_size, 512}).

-export([
    active/2,
    disable/2,
    enable/2,
    init/2,
    terminate/1
]).

%% public
-spec active(pool_name(), server_index()) ->
    boolean().

active(PoolName, ServerIndex) ->
    case counters(PoolName) of
        undefined ->
            false;
        Counters ->
            boolean(counters:get(Counters, ServerIndex))
    end.

-spec disable(pool_name(), server_index()) ->
    ok.

disable(PoolName, ServerIndex) ->
    case counters(PoolName) of
        undefined ->
            ok;
        Counters ->
            counters:sub(Counters, ServerIndex, 1)
    end.

-spec enable(pool_name(), server_index()) ->
    ok.

enable(PoolName, ServerIndex) ->
    case counters(PoolName) of
        undefined ->
            ok;
        Counters ->
            counters:add(Counters, ServerIndex, 1)
    end.

-spec init(pool_name(), server_index()) ->
    ok.

init(PoolName, PoolSize) ->
    Counters = counters:new(PoolSize, []),
    persistent_term:put(PoolName, Counters).

-spec terminate(pool_name()) ->
    ok.

terminate(PoolName) ->
    persistent_term:erase(PoolName),
    ok.

%% private
boolean(0) ->
    false;
boolean(1) ->
    true.

counters(PoolName) ->
    persistent_term:get(PoolName, undefined).
