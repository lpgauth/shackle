-module(shackle_backlog).
-include("shackle.hrl").

%% internal
-export([
    check/1,
    decrement/1,
    init/0,
    new/1
]).

%% internal
-spec check(atom()) -> boolean().

check(Key) ->
    MaxBacklogSize = backlog_size(),
    case increment(Key) of
        [MaxBacklogSize, MaxBacklogSize] ->
            false;
        [_, Value] when Value =< MaxBacklogSize ->
            true;
        {error, tid_missing} ->
            false
    end.

-spec decrement(atom()) -> non_neg_integer() | {error, tid_missing}.

decrement(Key) ->
    safe_update_counter(Key, {2, -1, 0, 0}).

-spec init() -> ?ETS_TABLE_BACKLOG.
init() ->
    ets:new(?ETS_TABLE_BACKLOG, [
        named_table,
        public,
        {write_concurrency, true}
    ]).

-spec new(atom()) -> true.

new(Key) ->
    ets:insert(?ETS_TABLE_BACKLOG, {Key, 0}).

%% private
increment(Key) ->
    MaxBacklogSize = backlog_size(),
    safe_update_counter(Key, [{2, 0}, {2, 1, MaxBacklogSize, MaxBacklogSize}]).

backlog_size() ->
    application:get_env(?APP, backlog_size, ?DEFAULT_BACKLOG_SIZE).

safe_update_counter(Key, UpdateOp) ->
    try ets:update_counter(?ETS_TABLE_BACKLOG, Key, UpdateOp)
    catch
        error:badarg ->
            {error, tid_missing}
    end.
