-module(shackle).
-include("shackle_internal.hrl").

%% public
-export([
    call/2,
    call/3,
    cast/3,
    receive_response/2
]).

%% public
-spec call(pool_name(), term()) -> {ok, term()} | {error, term()}.

call(PoolName, Call) ->
    call(PoolName, Call, ?DEFAULT_TIMEOUT).

-spec call(atom(), term(), timeout()) -> {ok, term()} | {error, term()}.

call(PoolName, Call, Timeout) ->
    case cast(PoolName, Call, self()) of
        {ok, RequestId} ->
            receive_response(RequestId, Timeout);
        {error, Reason} ->
            {error, Reason}
    end.

-spec cast(pool_name(), term(), pid()) -> {ok, shackle_req_id()} | {error, backlog_full}.

cast(PoolName, Cast, Pid) ->
    Timestamp = os:timestamp(),
    case shackle_pool:server(PoolName) of
        {ok, Client, Server} ->
            Ref = make_ref(),
            Request = #shackle_req {
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

-spec receive_response(shackle_req_id(), timeout()) ->
    {ok, reference()} | {error, term()}.

receive_response({PoolName, Client, Ref} = RequestId, Timeout) ->
    Timestamp = os:timestamp(),
    receive
        #shackle_req {
            pool_name = PoolName,
            ref = Ref
        } = Request ->

            process_timings(Client, Request),
            Request#shackle_req.reply;
        #shackle_req {pool_name = PoolName} ->
            Timeout2 = shackle_utils:timeout(Timeout, Timestamp),
            receive_response(RequestId, Timeout2)
    after Timeout ->
        {error, timeout}
    end.

%% private
process_timings(Client, #shackle_req {
        cast = Cast,
        timestamp = Timestamp,
        timings = Timings
    }) ->

    Timings2 = shackle_utils:timings(Timestamp, Timings),
    Timings3 = lists:reverse(Timings2),
    Client:process_timings(Cast, Timings3).
