-module(shackle_backlog).
-include("shackle_internal.hrl").

-compile(inline).
-compile({inline_size, 512}).

%% internal
-export([
    check/2,
    decrement/1,
    delete/1,
    init/0,
    new/1
]).

%% internal
-spec check(server_name(), backlog_size()) ->
    boolean().

check(_ServerName, infinity) ->
    true;
check(ServerName, BacklogSize) ->
    case increment(ServerName, BacklogSize) of
        [BacklogSize, BacklogSize] ->
            false;
        [_, Value] when Value =< BacklogSize ->
            true
    end.

-spec decrement(server_name() | request_id()) ->
    non_neg_integer().

decrement({ServerName, _}) ->
    decrement(ServerName);
decrement(ServerName) ->
    ets:update_counter(?ETS_TABLE_BACKLOG, ServerName, {2, -1, 0, 0}).

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
increment(ServerName, BacklogSize) ->
    UpdateOps = [{2, 0}, {2, 1, BacklogSize, BacklogSize}],
    ets:update_counter(?ETS_TABLE_BACKLOG, ServerName, UpdateOps).
