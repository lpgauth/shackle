-module(shackle_queue).

%% internal
-export([
    add/5,
    clear/2,
    delete/1,
    new/1,
    remove/3,
    table_name/1,
    pending/2,
    length/1,
    empty/2
]).

%% internal
-spec add(shackle:table(), shackle_server:id(), shackle:external_request_id(), shackle:cast(), reference()) ->
    ok.

add(Table, ServerId, ExtRequestId, Cast, TimerRef) ->
    Object = {{ServerId, ExtRequestId}, {Cast, TimerRef}},
    ets:insert(Table, Object),
    ok.

-spec clear(shackle:table(), shackle_server:id()) ->
    [{shackle:cast(), reference()}].

clear(Table, ServerId) ->
    Match = {{ServerId, '_'}, '_'},
    case ets_match_take(Table, Match) of
        [] ->
            [];
        Objects ->
            [{Cast, TimerRef} || {_, {Cast, TimerRef}} <- Objects]
    end.

-spec delete(shackle_pool:name()) ->
    ok.

delete(PoolName) ->
    ets:delete(table_name(PoolName)),
    ok.

-spec new(shackle_pool:name()) ->
    atom().

new(PoolName) ->
    TabName = table_name(PoolName),
    TabName = ets:new(TabName, shackle_utils:ets_options()),
    ets:give_away(TabName, whereis(shackle_ets_manager), undefined),
    TabName.

-spec remove(shackle:table(), shackle_server:id(), shackle:external_request_id()) ->
    {ok, shackle:cast(), reference()} | {error, not_found}.

remove(Table, ServerId, ExtRequestId) ->
    case ets_take(Table, {ServerId, ExtRequestId}) of
        [] ->
            {error, not_found};
        [{_, {Cast, TimerRef}}] ->
            {ok, Cast, TimerRef}
    end.

-spec pending(Table :: shackle:table(), ServerId :: shackle_server:id()) ->
    [{term(), {shackle:cast(), reference()}}].
pending(Table, ServerId) ->
    Match = {{ServerId, '_'}, '_'},
    ets:match_object(Table, Match).

-doc "Return the current length of the pending queue for all connections".
-spec length(Table :: shackle:table()) -> non_neg_integer().
length(Table) ->
    ets:info(Table, size).

-doc "Check if the queue for a single connection is empty".
-spec empty(Table :: shackle:table(), ServerId :: shackle_server:id()) ->
    boolean().
empty(Table, ServerId) ->
    '$end_of_table' == ets:select(Table, [{{{ServerId, '_'}, '_'}, [], [1]}], 1).

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
