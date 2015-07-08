-module(shackle).
-include("shackle.hrl").

-export([
    cast/4,
    call/4,
    child_name/2,
    child_specs/2,
    receive_response/3
]).

%% public
cast(Namespace, Msg, Pid, PoolSize) ->
    Ref = make_ref(),
    Server = random_server(Namespace, PoolSize),
    case anchor_backlog:check(Server) of
        true ->
            Server ! {call, Ref, Pid, Msg},
            {ok, Ref};
        false ->
            {error, backlog_full}
    end.

call(Namespace, Msg, Timeout, PoolSize) ->
    case cast(Namespace, Msg, self(), PoolSize) of
        {ok, Ref} ->
            receive_response(Namespace, Ref, Timeout);
        {error, Reason} ->
            {error, Reason}
    end.

child_specs(Namespace, PoolSize) ->
    [?CHILD(child_name(Namespace, N), ?SERVER) || N <- lists:seq(1, PoolSize)].

receive_response(Namespace, Ref, Timeout) ->
    Timestamp = os:timestamp(),
    receive
        {Namespace, Ref, Reply} ->
            Reply;
        {Namespace, _, _} ->
            Timeout2 = timeout(Timeout, Timestamp),
            receive_response(Namespace, Ref, Timeout2)
    after Timeout ->
        {error, timeout}
    end.

%% private
child_name(Namespace, N) ->
    list_to_atom(Namespace ++ integer_to_list(N)).

random_server(Namespace, PoolSize) ->
    Random = erlang:phash2({os:timestamp(), self()}, PoolSize) + 1,
    child_name(Namespace, Random).

timeout(Timeout, Timestamp) ->
    Diff = timer:now_diff(os:timestamp(), Timestamp) div 1000,
    Timeout - Diff.
