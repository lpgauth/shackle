-module(shackle_queue).
-include("shackle_internal.hrl").

-compile(inline).
-compile({inline_size, 512}).

%% internal
-export([
    add/5,
    clear/2,
    delete/1,
    new/1,
    remove/3,
    table_name/1
]).

%% internal
-spec add(table(), server_id(), external_request_id(), cast(), reference()) ->
    ok.

add(Table, ServerId, ExtRequestId, Cast, TimerRef) ->
    Object = {{ServerId, ExtRequestId}, {Cast, TimerRef}},
    ets:insert(Table, Object),
    ok.

-spec clear(table(), server_id()) ->
    [{cast(), reference()}].

clear(Table, ServerId) ->
    Match = {{ServerId, '_'}, '_'},
    case ets_match_take(Table, Match) of
        [] ->
            [];
        Objects ->
            [{Cast, TimerRef} || {_, {Cast, TimerRef}} <- Objects]
    end.

-spec delete(pool_name()) ->
    ok.

delete(PoolName) ->
    ets:delete(table_name(PoolName)),
    ok.

-spec new(pool_name()) ->
    ok.

new(PoolName) ->
    Table = ets:new(table_name(PoolName), shackle_utils:ets_options()),
    ets:give_away(Table, whereis(shackle_ets_manager), undefined),
    ok.

-spec remove(table(), server_id(), external_request_id()) ->
    {ok, cast(), reference()} | {error, not_found}.

remove(Table, ServerId, ExtRequestId) ->
    case ets_take(Table, {ServerId, ExtRequestId}) of
        [] ->
            {error, not_found};
        [{_, {Cast, TimerRef}}] ->
            {ok, Cast, TimerRef}
    end.

%% private
ets_match_take(Table, Match) ->
    case ets:match_object(Table, Match) of
        [] ->
            [];
        Objects ->
            ets:match_delete(Table, Match),
            Objects
    end.

-ifdef(ETS_TAKE).

ets_take(Table, Key) ->
    ets:take(Table, Key).

-else.

ets_take(Table, Key) ->
    case ets:lookup(Table, Key) of
        [] ->
            [];
        Objects ->
            ets:delete(Table, Key),
            Objects
    end.

-endif.

-spec table_name(pool_name()) ->
    table().

table_name(PoolName) ->
    list_to_atom("shackle_queue_" ++ atom_to_list(PoolName)).
