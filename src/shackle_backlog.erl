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
-spec check(server_name(), backlog_size()) ->
    boolean().

check(ServerName, BacklogSize) ->
    check(ServerName, BacklogSize, ?DEFAULT_INCREMENT).

-spec check(server_name(), backlog_size(), pos_integer()) ->
    boolean().

check(_ServerName, infinity, _Increment) ->
    true;
check(ServerName, BacklogSize, Increment) ->
    case increment(ServerName, BacklogSize, Increment) of
        [BacklogSize, BacklogSize] ->
            false;
        [_, Value] when Value =< BacklogSize ->
            true
    end.

-spec decrement(server_name()) ->
    non_neg_integer().

decrement(ServerName) ->
    decrement(ServerName, ?DEFAULT_DECREMENT).

-spec decrement(server_name(), neg_integer()) ->
    non_neg_integer().

decrement(ServerName, Decrement) ->
    ets:update_counter(?ETS_TABLE_BACKLOG, ServerName, {2, Decrement, 0, 0}).

-spec delete(server_name()) ->
    ok.

delete(ServerName) ->
    ets:delete(?ETS_TABLE_BACKLOG, ServerName),
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

-spec new(server_name()) ->
    ok.

new(ServerName) ->
    ets:insert(?ETS_TABLE_BACKLOG, {ServerName, 0}),
    ok.

%% private
increment(ServerName, BacklogSize, Increment) ->
    UpdateOps = [{2, 0}, {2, Increment, BacklogSize, BacklogSize}],
    ets:update_counter(?ETS_TABLE_BACKLOG, ServerName, UpdateOps).
