-module(shackle_queue).
-include("shackle_internal.hrl").

%% internal
-export([
    all/1,
    in/3,
    init/0,
    out/2,
    remove/1
]).

%% internal
-spec all(server_name()) -> [cast()].

all(ServerName) ->
    Match = {{ServerName, '_'}, '_'},
    Matches = ets:match_object(?ETS_TABLE_QUEUE, Match),
    ets:match_delete(?ETS_TABLE_QUEUE, Match),
    [Request || {_, Request} <- Matches].

-spec init() -> ?ETS_TABLE_QUEUE.

init() ->
    ets:new(?ETS_TABLE_QUEUE, [
        named_table,
        public,
        {read_concurrency, true},
        {write_concurrency, true}
    ]).

-spec in(server_name(), external_request_id(), cast()) -> ok.

in(ServerName, RequestId, Request) ->
    ets:insert(?ETS_TABLE_QUEUE, {{ServerName, RequestId}, Request}),
    ok.

-spec out(atom(), external_request_id()) -> {ok, cast()} | {error, not_found}.

out(ServerName, RequestId) ->
    Key = {ServerName, RequestId},
    try
        % TODO: use ets:take/2
        Request = ets:lookup_element(?ETS_TABLE_QUEUE, Key, 2),
        ets:delete(?ETS_TABLE_QUEUE, Key),
        {ok, Request}
    catch
        error:badarg ->
            {error, not_found}
    end.

-spec remove(external_request_id()) -> true.

remove(RequestId) ->
    Match = {'_', {cast, RequestId, '_', '_', '_', '_', '_', '_'}},
    ets:match_delete(?ETS_TABLE_QUEUE, Match).
