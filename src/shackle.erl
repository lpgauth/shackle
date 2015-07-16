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
-spec call(atom(), term()) -> {ok, term()} | {error, term()}.

call(Name, Msg) ->
    call(Name, Msg, ?DEFAULT_TIMEOUT).

-spec call(atom(), term(), pos_integer()) -> {ok, term()} | {error, term()}.

call(Name, Msg, Timeout) ->
    case cast(Name, Msg, self()) of
        {ok, Ref} ->
            receive_response(Name, Ref, Timeout);
        {error, Reason} ->
            {error, Reason}
    end.

-spec cast(atom(), term(), pid()) -> {ok, reference()} | {error, backlog_full}.

cast(Name, Msg, Pid) ->
    case shackle_pool:server(Name) of
        {ok, Server} ->
            Ref = make_ref(),
            Server ! {call, Ref, Pid, Msg},
            {ok, Ref};
        {error, backlog_full} ->
            {error, backlog_full}
    end.

-spec receive_response(module(), reference(), pos_integer()) ->
    {ok, reference()} | {error, term()}.

receive_response(Name, Ref, Timeout) ->
    Timestamp = os:timestamp(),
    receive
        {Name, Ref, Reply} ->
            Reply;
        {Name, _, _} ->
            Timeout2 = shackle_utils:timeout(Timeout, Timestamp),
            receive_response(Name, Ref, Timeout2)
    after Timeout ->
        {error, timeout}
    end.
