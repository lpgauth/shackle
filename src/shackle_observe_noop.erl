-module(shackle_observe_noop).

-behaviour(shackle_observe_dispatcher).

-doc """
No-op observability dispatcher used when no backend is configured.
Each function calls through directly without any span wrapping or event emission.
Returned by `shackle_observe:dispatcher/0` when `enabled/0` is false.
""".

-export([
    call/2,
    cast/2,
    connect/2,
    timeout/2,
    error/2
]).

-spec call(atom(), fun(() -> Result)) -> Result when Result :: term().
call(_PoolName, Fun) ->
    Fun().

-spec cast(atom(), fun(() -> Result)) -> Result when Result :: term().
cast(_PoolName, Fun) ->
    Fun().

-spec connect(atom(), fun(() -> Result)) -> Result when Result :: term().
connect(_PoolName, Fun) ->
    Fun().

-spec timeout(atom(), fun(() -> Result)) -> Result when Result :: term().
timeout(_PoolName, Fun) ->
    Fun().

-spec error(atom(), fun(() -> Result)) -> Result when Result :: term().
error(_PoolName, Fun) ->
    Fun().
