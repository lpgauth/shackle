-module(shackle_cache).
-include("shackle.hrl").

%% public
-export([
    erase/1,
    get/1,
    put/2
]).

%% internal
-export([
    init/0
]).

%% public
-spec erase(binary()) -> true | {error, not_found}.

erase(Key) ->
    try ets:delete(?ETS_TABLE_CACHE, Key)
    catch
        error:badarg ->
            {error, not_found}
    end.

-spec get(binary()) -> {ok, term()} | {error, not_found}.

get(Key) ->
    try
        Term = ets:lookup_element(?ETS_TABLE_CACHE, Key, 2),
        {ok, Term}
    catch
        error:badarg ->
            {error, not_found}
    end.

-spec put(binary(), term()) -> true.

put(Key, Value) ->
    ets:insert(?ETS_TABLE_CACHE, {Key, Value}).

%% internal
-spec init() -> ?ETS_TABLE_CACHE.

init() ->
    ets:new(?ETS_TABLE_CACHE, [
        named_table,
        public,
        {read_concurrency, true}
    ]).
