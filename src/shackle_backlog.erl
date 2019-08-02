-module(shackle_backlog).
-include("shackle_internal.hrl").

-compile(inline).
-compile({inline_size, 512}).

%% internal
-export([
    check/2,
    check/3,
    decrement/1,
    decrement/2,
    delete/1,
    init/0,
    new/1
]).

-define(DEFAULT_DECREMENT, -1).
-define(DEFAULT_INCREMENT, 1).

%% internal
-spec check(server_id(), backlog_size()) ->
    boolean().

check(ServerId, BacklogSize) ->
    check(ServerId, BacklogSize, ?DEFAULT_INCREMENT).

-spec check(server_id(), backlog_size(), pos_integer()) ->
    boolean().

check(_ServerId, infinity, _Increment) ->
    true;
check(ServerId, BacklogSize, Increment) ->
    case increment(ServerId, BacklogSize, Increment) of
        [BacklogSize, BacklogSize] ->
            false;
        [_, Value] when Value =< BacklogSize ->
            true
    end.

-spec decrement(server_id()) ->
    non_neg_integer().

decrement(ServerId) ->
    decrement(ServerId, ?DEFAULT_DECREMENT).

-spec decrement(server_id(), neg_integer()) ->
    non_neg_integer().

decrement(ServerId, Decrement) ->
    ets:update_counter(?ETS_TABLE_BACKLOG, ServerId, {2, Decrement, 0, 0}).

-spec delete(server_id()) ->
    ok.

delete(ServerId) ->
    ets:delete(?ETS_TABLE_BACKLOG, ServerId),
    ok.

-spec init() ->
    ok.

init() ->
    ets:new(?ETS_TABLE_BACKLOG, [
        named_table,
        public,
        {write_concurrency, true}
    ]),
    ok.

-spec new(server_id()) ->
    ok.

new(ServerId) ->
    ets:insert(?ETS_TABLE_BACKLOG, {ServerId, 0}),
    ok.

%% private
increment(ServerId, BacklogSize, Increment) ->
    UpdateOps = [{2, 0}, {2, Increment, BacklogSize, BacklogSize}],
    ets:update_counter(?ETS_TABLE_BACKLOG, ServerId, UpdateOps).
