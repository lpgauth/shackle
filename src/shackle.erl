% TODO: add specs
% TODO: store pool info in ETS (size, selection)

-module(shackle).
-include("shackle.hrl").

-export([
    cast/4,
    call/4,
    receive_response/3,
    start/2
]).

%% public
cast(Namespace, Msg, Pid, PoolSize) ->
    Ref = make_ref(),
    Server = random_server(Namespace, PoolSize),
    case shackle_backlog:check(Server) of
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

start(_Module, _PoolSize) ->
    % % supervisor:start_link({local, ?MODULE}, ?MODULE, []).
    % child_specs(Module, PoolSize).
    ok.

%% private
random_server(Namespace, PoolSize) ->
    % TODO: round_robin
    Random = erlang:phash2({os:timestamp(), self()}, PoolSize) + 1,
    shackle_utils:child_name(Namespace, Random).
