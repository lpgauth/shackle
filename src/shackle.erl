-module(shackle).
-include("shackle_internal.hrl").

%% public
-export([
    call/2,
    call/3,
    cast/2,
    cast/3,
    cast/4,
    handle_timing/1,
    receive_response/1
]).

%% public
-spec call(pool_name(), term()) -> term() | {error, term()}.

call(PoolName, Request) ->
    call(PoolName, Request, ?DEFAULT_TIMEOUT).

-spec call(atom(), term(), timeout()) -> term() | {error, term()}.

call(PoolName, Request, Timeout) ->
    case cast(PoolName, Request, self(), Timeout) of
        {ok, RequestId} ->
            receive_response(RequestId);
        {error, Reason} ->
            {error, Reason}
    end.

-spec cast(pool_name(), term()) -> {ok, request_id()} | {error, backlog_full}.

cast(PoolName, Request) ->
    cast(PoolName, Request, self()).

-spec cast(pool_name(), term(), pid() | undefined) ->
    {ok, request_id()} | {error, backlog_full}.

cast(PoolName, Request, Pid) ->
    cast(PoolName, Request, Pid, ?DEFAULT_TIMEOUT).

-spec cast(pool_name(), term(), pid() | undefined, timeout()) ->
    {ok, request_id()} | {error, backlog_full}.

cast(PoolName, Request, Pid, Timeout) ->
    Timestamp = os:timestamp(),
    case shackle_pool:server(PoolName) of
        {ok, Client, Server, Manager} ->
            RequestId = {Server, make_ref()},
            TimerRef = timeout(Manager, RequestId, Pid, Timeout),
            Server ! #cast {
                client = Client,
                pid = Pid,
                request = Request,
                request_id = RequestId,
                timeout = Timeout,
                timer_ref = TimerRef,
                timestamp = Timestamp
            },
            {ok, RequestId};
        {error, backlog_full} ->
            {error, backlog_full}
    end.

-spec handle_timing(cast()) -> ok.

handle_timing(#cast {
        client = Client,
        request = Request,
        timestamp = Timestamp,
        timing = Timing
    }) ->

    Timing2 = shackle_utils:timing(Timestamp, Timing),
    Timing3 = lists:reverse(Timing2),
    Client:handle_timing(Request, Timing3).

-spec receive_response(request_id()) ->
    term() | {error, term()}.

receive_response(RequestId) ->
    receive
        #cast {request_id = RequestId, reply = Reply} = Cast ->
            handle_timing(Cast),
            Reply
    end.

%% private
timeout(_Manager, _RequestId, _Pid, undefined) ->
    undefined;
timeout(Manager, RequestId, Pid, Timeout) ->
    Msg = {timeout, RequestId, Pid},
    erlang:send_after(Timeout, Manager, Msg).
