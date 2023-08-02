-module(shackle_queue).

-compile(inline).
-compile({inline_size, 512}).

%% internal
-export([
    add/6,
    clear/2,
    delete/1,
    new/1,
    remove/3,
    table_name/1
]).

%% internal
-spec add(shackle:table(), shackle_server:id(), shackle:external_request_id(), term(), shackle:cast(), reference()) ->
    ok.

add(Table, ServerId, ExtRequestId, Request, Cast, TimerRef) ->
    Object = {{ServerId, ExtRequestId}, {Cast, Request, TimerRef}},
    ets:insert(Table, Object),
    ok.

-spec clear(shackle:table(), shackle_server:id()) ->
    [{shackle:cast(), term(), reference()}].

clear(Table, ServerId) ->
    Match = {{ServerId, '_'}, '_'},
    case ets_match_take(Table, Match) of
        [] ->
            [];
        Objects ->
            [{Cast, Request, TimerRef} || {_, {Cast, Request, TimerRef}} <- Objects]
    end.

-spec delete(shackle_pool:name()) ->
    ok.

delete(PoolName) ->
    ets:delete(table_name(PoolName)),
    ok.

-spec new(shackle_pool:name()) ->
    ok.

new(PoolName) ->
    Table = ets:new(table_name(PoolName), shackle_utils:ets_options()),
    ets:give_away(Table, whereis(shackle_ets_manager), undefined),
    ok.

-spec remove(shackle:table(), shackle_server:id(), shackle:external_request_id()) ->
    {ok, shackle:cast(), term(), reference()} | {error, not_found}.

remove(Table, ServerId, ExtRequestId) ->
    case ets_take(Table, {ServerId, ExtRequestId}) of
        [] ->
            {error, not_found};
        [{_, {Cast, Request, TimerRef}}] ->
            {ok, Cast, Request, TimerRef}
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

-spec table_name(shackle_pool:name()) ->
    shackle:table().

table_name(PoolName) ->
    list_to_atom("shackle_queue_" ++ atom_to_list(PoolName)).
