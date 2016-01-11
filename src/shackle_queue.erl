%% TODO: benchmark ETS read_concurrency / write_concurrency

-module(shackle_queue).
-include("shackle_internal.hrl").

%% internal
-export([
    add/2,
    clear/1,
    init/0,
    remove/1,
    remove/2
]).

%% internal
-spec add(external_request_id(), cast()) ->
    ok.

add(ExtRequestId, #cast {request_id = RequestId} = Cast) ->
    Object = {{RequestId, ExtRequestId}, Cast},
    ets:insert(?ETS_TABLE_QUEUE, Object),
    ok.

-spec clear(server_name()) ->
    [cast()].

clear(ServerName) ->
    Match = {{{ServerName, '_'}, '_'}, '_'},
    case match_take(Match) of
        [] ->
            [];
        Objects ->
            [Cast || {_, Cast} <- Objects]
    end.

-spec init() ->
    ?ETS_TABLE_QUEUE.

init() ->
    ets:new(?ETS_TABLE_QUEUE, [
        named_table,
        public,
        {read_concurrency, true},
        {write_concurrency, true}
    ]).

-spec remove(request_id()) ->
    {ok, cast()} | {error, not_found}.

remove(RequestId) ->
    Match = {{RequestId, '_'}, '_'},
    case match_take(Match) of
        [] ->
            {error, not_found};
        [{_, Cast}] ->
            {ok, Cast}
    end.

-spec remove(server_name(), external_request_id()) ->
    {ok, cast()} | {error, not_found}.

remove(ServerName, ExtRequestId) ->
    Match = {{{ServerName, '_'}, ExtRequestId}, '_'},
    case match_take(Match) of
        [] ->
            {error, not_found};
        [{_, Cast}] ->
            {ok, Cast}
    end.

%% private
match_take(Match) ->
    case ets:match_object(?ETS_TABLE_QUEUE, Match) of
        [] ->
            [];
        Objects ->
            ets:match_delete(?ETS_TABLE_QUEUE, Match),
            Objects
    end.
