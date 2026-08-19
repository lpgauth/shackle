-module(shackle_observe_span).

-behaviour(shackle_observe_dispatcher).

-doc """
Observability dispatcher used when a backend is configured.
Each function wraps the operation in `shackle_observe:span/3`, emitting start/stop events.
Returned by `shackle_observe:dispatcher/0` when `enabled/0` is true.
""".

-export([
    call/2,
    cast/2,
    connect/2,
    timeout/2,
    error/2
]).

-spec call(atom(), fun(() -> Result)) -> Result when Result :: term().
call(PoolName, Fun) ->
    shackle_observe:span([call], #{pool => PoolName}, Fun).

-spec cast(atom(), fun(() -> Result)) -> Result when Result :: term().
cast(PoolName, Fun) ->
    shackle_observe:span([cast], #{pool => PoolName}, Fun).

-spec connect(atom(), fun(() -> Result)) -> Result when Result :: term().
connect(PoolName, Fun) ->
    shackle_observe:span([connect], #{pool => PoolName}, Fun).

-spec timeout(atom(), fun(() -> Result)) -> Result when Result :: term().
timeout(PoolName, Fun) ->
    shackle_observe:span([timeout], #{pool => PoolName}, Fun).

-spec error(atom(), fun(() -> Result)) -> Result when Result :: term().
error(PoolName, Fun) ->
    shackle_observe:span([error], #{pool => PoolName}, Fun).
