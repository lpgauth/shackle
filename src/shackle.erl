% TODO: add specs
% TODO: store pool info in ETS (size, selection)

-module(shackle).
-include("shackle.hrl").

-export([
    call/4,
    cast/4,
    receive_response/3,
    start_pool/2,
    stop_pool/2
]).

%% public
call(Namespace, Msg, Timeout, PoolSize) ->
    case cast(Namespace, Msg, self(), PoolSize) of
        {ok, Ref} ->
            receive_response(Namespace, Ref, Timeout);
        {error, Reason} ->
            {error, Reason}
    end.

cast(Namespace, Msg, Pid, PoolSize) ->
    Ref = make_ref(),
    Server = random_server(Namespace, PoolSize),
    % TODO: fix me
    BacklogSize = 1024,
    case shackle_backlog:check(Server, BacklogSize) of
        true ->
            Server ! {call, Ref, Pid, Msg},
            {ok, Ref};
        false ->
            {error, backlog_full}
    end.

receive_response(Namespace, Ref, Timeout) ->
    Timestamp = os:timestamp(),
    receive
        {Namespace, Ref, Reply} ->
            Reply;
        {Namespace, _, _} ->
            Timeout2 = shackle_utils:timeout(Timeout, Timestamp),
            receive_response(Namespace, Ref, Timeout2)
    after Timeout ->
        {error, timeout}
    end.

start_pool(Module, PoolSize) ->
    ChildNames = shackle_utils:child_names(Module, PoolSize),
    ChildSpecs = [?CHILD(ChildName, Module) || ChildName <- ChildNames],
    [supervisor:start_child(?SUPERVISOR, ChildSpec) || ChildSpec <- ChildSpecs].

% TODO: fix me
stop_pool(Module, PoolSize) ->
    ChildNames = shackle_utils:child_names(Module, PoolSize),
    [supervisor:delete_child(?SUPERVISOR, ChildName) || ChildName <- ChildNames].

%% private
% TODO: random or round_robin
random_server(Namespace, PoolSize) ->
    Random = erlang:phash2({os:timestamp(), self()}, PoolSize) + 1,
    shackle_utils:child_name(Namespace, Random).
