-module(shackle).
-include("shackle.hrl").

%% public
-export([
    call/2,
    call/3,
    cast/3,
    receive_response/2
]).

%% public
-spec call(pool_name(), term()) -> {ok, term()} | {error, term()}.

call(PoolName, Request) ->
    call(PoolName, Request, ?DEFAULT_TIMEOUT).

-spec call(atom(), term(), timeout()) -> {ok, term()} | {error, term()}.

call(PoolName, Call, Timeout) ->
    case cast(PoolName, Call, self()) of
        {ok, RequestId} ->
            receive_response(RequestId, Timeout);
        {error, Reason} ->
            {error, Reason}
    end.

-spec cast(pool_name(), term(), pid()) -> {ok, request_id()} | {error, backlog_full}.

cast(PoolName, Cast, Pid) ->
    Timestamp = os:timestamp(),
    case shackle_pool:server(PoolName) of
        {ok, Client, Server} ->
            Ref = make_ref(),
            Request = #request {
                cast = Cast,
                from = Pid,
                pool_name = PoolName,
                ref = Ref,
                timestamp = Timestamp
            },
            Server ! {call, Request},
            {ok, {PoolName, Client, Ref}};
        {error, backlog_full} ->
            {error, backlog_full}
    end.

-spec receive_response(request_id(), timeout()) ->
    {ok, reference()} | {error, term()}.

receive_response({PoolName, Client, Ref} = RequestId, Timeout) ->
    Timestamp = os:timestamp(),
    receive
        #request {
            pool_name = PoolName,
            ref = Ref,
            timestamp = Timestamp2,
            timings = Timings
        } = Request ->

            Timing = shackle_utils:now_diff(Timestamp2),
            Client:process_timings(lists:reverse([Timing | Timings])),
            Request#request.reply;
        #request {pool_name = PoolName} ->
            Timeout2 = shackle_utils:timeout(Timeout, Timestamp),
            receive_response(RequestId, Timeout2)
    after Timeout ->
        {error, timeout}
    end.
