-module(shackle_backlog).
-include("shackle_internal.hrl").

-compile(inline).
-compile({inline_size, 512}).

%% internal
-export([
    check/3,
    check/4,
    decrement/2,
    decrement/3,
    delete/1,
    delete/2,
    new/1,
    new/2,
    table_name/1
]).

-define(DEFAULT_DECREMENT, -1).
-define(DEFAULT_INCREMENT, 1).

%% internal
-spec check(table(), server_id(), backlog_size()) ->
    boolean().

check(Table, ServerId, BacklogSize) ->
    check(Table, ServerId, BacklogSize, ?DEFAULT_INCREMENT).

-spec check(table(), server_id(), backlog_size(), pos_integer()) ->
    boolean().

check(_Table, _ServerId, infinity, _Increment) ->
    true;
check(Table, ServerId, BacklogSize, Increment) ->
    case increment(Table, ServerId, BacklogSize, Increment) of
        [BacklogSize, BacklogSize] ->
            false;
        [_, Value] when Value =< BacklogSize ->
            true
    end.

-spec decrement(table(), server_id()) ->
    non_neg_integer().

decrement(Table, ServerId) ->
    decrement(Table, ServerId, ?DEFAULT_DECREMENT).

-spec decrement(table(), server_id(), neg_integer()) ->
    non_neg_integer().

decrement(Table, ServerId, Decrement) ->
    ets:update_counter(Table, ServerId, {2, Decrement, 0, 0}).

-spec delete(pool_name()) ->
    ok.

delete(PoolName) ->
    ets:delete(table_name(PoolName)),
    ok.

-spec delete(pool_name(), server_id()) ->
    ok.

delete(PoolName, ServerId) ->
    ets:delete(table_name(PoolName), ServerId),
    ok.

-spec new(pool_name()) ->
    ok.

new(PoolName) ->
    Table = ets:new(table_name(PoolName), [
        named_table,
        public,
        {write_concurrency, true}
    ]),
    ets:give_away(Table, whereis(shackle_ets_manager), undefined),
    ok.

-spec new(pool_name(), server_id()) ->
    ok.

new(PoolName, ServerId) ->
    ets:insert(table_name(PoolName), {ServerId, 0}),
    ok.

-spec table_name(pool_name()) ->
    table().

table_name(PoolName) ->
    list_to_atom("shackle_backlog_" ++ atom_to_list(PoolName)).

%% private
increment(Table, ServerId, BacklogSize, Increment) ->
    UpdateOps = [{2, 0}, {2, Increment, BacklogSize, BacklogSize}],
    ets:update_counter(Table, ServerId, UpdateOps).
