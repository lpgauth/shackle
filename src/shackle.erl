-module(shackle).
-include("shackle.hrl").

%% public
-export([
    call/2,
    call/3,
    cast/3,
    receive_response/3
]).

%% public
-spec call(pool_name(), request()) -> {ok, term()} | {error, term()}.

call(PoolName, Request) ->
    call(PoolName, Request, ?DEFAULT_TIMEOUT).

-spec call(atom(), request(), timeout()) -> {ok, term()} | {error, term()}.

call(PoolName, Request, Timeout) ->
    case cast(PoolName, Request, self()) of
        {ok, Ref} ->
            receive_response(PoolName, Ref, Timeout);
        {error, Reason} ->
            {error, Reason}
    end.

-spec cast(pool_name(), request(), pid()) -> {ok, reference()} | {error, backlog_full}.

cast(PoolName, Request, Pid) ->
    case shackle_pool:server(PoolName) of
        {ok, Server} ->
            Ref = make_ref(),
            Server ! {call, Ref, Pid, Request},
            {ok, Ref};
        {error, backlog_full} ->
            {error, backlog_full}
    end.

-spec receive_response(pool_name(), reference(), timeout()) ->
    {ok, reference()} | {error, term()}.

receive_response(PoolName, Ref, Timeout) ->
    Timestamp = os:timestamp(),
    receive
        {PoolName, Ref, Reply} ->
            Reply;
        {PoolName, _, _} ->
            Timeout2 = shackle_utils:timeout(Timeout, Timestamp),
            receive_response(PoolName, Ref, Timeout2)
    after Timeout ->
        {error, timeout}
    end.
