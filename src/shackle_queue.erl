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
-spec all(atom()) -> [term()].

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

-spec in(atom(), non_neg_integer(), term()) -> true.

in(ServerName, Stream, Item) ->
    ets:insert(?ETS_TABLE_QUEUE, {{ServerName, Stream}, Item}).

-spec out(atom(), non_neg_integer()) -> term().

out(ServerName, Stream) ->
    Key = {ServerName, Stream},
    Item = ets:lookup_element(?ETS_TABLE_QUEUE, Key, 2),
    ets:delete(?ETS_TABLE_QUEUE, Key),
    Item.
