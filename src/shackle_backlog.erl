-module(shackle_backlog).
-include("shackle.hrl").

%% internal
-export([
    check/2,
    decrement/1,
    delete/1,
    init/0,
    new/1
]).

%% internal
-spec check(atom(), pos_integer()) -> boolean().

check(Key, BacklogSize) ->
    case increment(Key, BacklogSize) of
        [BacklogSize, BacklogSize] ->
            false;
        [_, Value] when Value =< BacklogSize ->
            true
    end.

-spec decrement(atom()) -> non_neg_integer().

decrement(Key) ->
    ets:update_counter(?ETS_TABLE_BACKLOG, Key, {2, -1, 0, 0}).

-spec delete(atom()) -> ok.

delete(Key) ->
    ets:delete(?ETS_TABLE_BACKLOG, Key),
    ok.

-spec init() -> ?ETS_TABLE_BACKLOG.

init() ->
    ets:new(?ETS_TABLE_BACKLOG, [
        named_table,
        public,
        {read_concurrency, true},
        {write_concurrency, true}
    ]).

-spec new(atom()) -> ok.

new(Key) ->
    ets:insert(?ETS_TABLE_BACKLOG, {Key, 0}),
    ok.

%% private
increment(Key, BacklogSize) ->
    UpdateOps = [{2, 0}, {2, 1, BacklogSize, BacklogSize}],
    ets:update_counter(?ETS_TABLE_BACKLOG, Key, UpdateOps).
