-module(shackle).
-include("shackle_internal.hrl").

%% public
-export([
    call/2,
    call/3,
    cast/2,
    cast/3,
    receive_response/1,
    receive_response/2
]).

%% public
-spec call(pool_name(), term()) -> term() | {error, term()}.

call(PoolName, Request) ->
    call(PoolName, Request, ?DEFAULT_TIMEOUT).

-spec call(atom(), term(), timeout()) -> term() | {error, term()}.

call(PoolName, Request, Timeout) ->
    case cast(PoolName, Request) of
        {ok, RequestId} ->
            receive_response(RequestId, Timeout);
        {error, Reason} ->
            {error, Reason}
    end.

-spec cast(pool_name(), term()) -> {ok, request_id()} | {error, backlog_full}.

cast(PoolName, Request) ->
    cast(PoolName, Request, self()).

-spec cast(pool_name(), term(), pid()) ->
    {ok, request_id()} | {error, atom()}.

cast(PoolName, Request, Pid) ->
    Timestamp = os:timestamp(),
    case shackle_pool:server(PoolName) of
        {ok, Client, Server} ->
            RequestId = {Server, make_ref()},
            Server ! #cast {
                client = Client,
                pid = Pid,
                request = Request,
                request_id = RequestId,
                timestamp = Timestamp
            },
            {ok, RequestId};
        {error, Reason} ->
            {error, Reason}
    end.

-spec receive_response(request_id()) ->
    term() | {error, term()}.

receive_response(RequestId) ->
    receive_response(RequestId, ?DEFAULT_TIMEOUT).

-spec receive_response(request_id(), timeout()) ->
    term() | {error, term()}.

receive_response(RequestId, Timeout) ->
    receive
        #cast {request_id = RequestId, reply = Reply} ->
            Reply
    after Timeout ->
        shackle_queue:remove(RequestId),
        shackle_backlog:decrement(RequestId),
        {error, timeout}
    end.
