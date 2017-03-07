-module(shackle).
-include("shackle_internal.hrl").

-compile(inline).
-compile({inline_size, 512}).

%% public
-export([
    call/2,
    call/3,
    cast/2,
    cast/3,
    cast/4,
    receive_response/1
]).

%% public
-spec call(pool_name(), term()) ->
    term() | {error, term()}.

call(PoolName, Request) ->
    call(PoolName, Request, ?DEFAULT_TIMEOUT).

-spec call(atom(), term(), timeout()) ->
    term() | {error, term()}.

call(PoolName, Request, Timeout) ->
    case cast(PoolName, Request, self(), Timeout) of
        {ok, RequestId} ->
            receive_response(RequestId);
        {error, Reason} ->
            {error, Reason}
    end.

-spec cast(pool_name(), term()) ->
    {ok, request_id()} | {error, atom()}.

cast(PoolName, Request) ->
    cast(PoolName, Request, self()).

-spec cast(pool_name(), term(), pid()) ->
    {ok, request_id()} | {error, atom()}.

cast(PoolName, Request, Pid) ->
    cast(PoolName, Request, Pid, ?DEFAULT_TIMEOUT).

-spec cast(pool_name(), term(), pid(), timeout()) ->
    {ok, request_id()} | {error, atom()}.

cast(PoolName, Request, Pid, Timeout) ->
    Timestamp = os:timestamp(),
    case shackle_pool:server(PoolName) of
        {ok, Client, Server} ->
            RequestId = {Server, make_ref()},
            Server ! #cast {
                client = Client,
                pid = Pid,
                request = Request,
                request_id = RequestId,
                timeout = Timeout,
                timestamp = Timestamp
            },
            {ok, RequestId};
        {error, Reason} ->
            {error, Reason}
    end.

-spec receive_response(request_id()) ->
    term() | {error, term()}.

receive_response(RequestId) ->
    receive
        {#cast {request_id = RequestId}, Reply} ->
            Reply
    end.
