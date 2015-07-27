-module(shackle_queue).
-include("shackle.hrl").

%% internal
-export([
    all/1,
    in/3,
    init/0,
    out/2
]).

%% internal
-spec all(server_name()) -> [term()].

all(ServerName) ->
    Match = {{ServerName, '_'}, '_'},
    Matches = ets:match_object(?ETS_TABLE_QUEUE, Match),
    ets:match_delete(?ETS_TABLE_QUEUE, Match),
    [Item || {_, Item} <- Matches].

-spec init() -> ?ETS_TABLE_QUEUE.

init() ->
    ets:new(?ETS_TABLE_QUEUE, [
        named_table,
        public,
        {read_concurrency, true},
        {write_concurrency, true}
    ]).

-spec in(server_name(), request_id(), term()) -> ok.

in(ServerName, RequestId, Item) ->
    ets:insert(?ETS_TABLE_QUEUE, {{ServerName, RequestId}, Item}),
    ok.

-spec out(atom(), request_id()) -> {ok, term()} | {error, not_found}.

out(ServerName, RequestId) ->
    Key = {ServerName, RequestId},
    try
        Item = ets:lookup_element(?ETS_TABLE_QUEUE, Key, 2),
        ets:delete(?ETS_TABLE_QUEUE, Key),
        {ok, Item}
    catch
        error:badarg ->
            {error, not_found}
    end.
