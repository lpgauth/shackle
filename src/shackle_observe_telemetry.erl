-module(shackle_observe_telemetry).

-moduledoc """
Shackle observability backend that forwards every event to `telemetry:execute/3`
unchanged, so any standard `telemetry:attach/4` handler works against shackle
exactly as it would for any other telemetry-instrumented library.

Selected via `{shackle, [{observability, telemetry}]}`. The `shackle_observe`
supervisor calls `start/1` once at boot and `stop/0` on shutdown; `start/1`
lazy-starts the telemetry application if it isn't running already, and `stop/0`
only stops it if this module is the one that started it (so a host application
that already has telemetry running keeps it running).

See `m:shackle_observe` for the full `[shackle | _]` event catalog this module
receives and forwards.
""".

-behaviour(shackle_observe).

-export([start/1, stop/0, event/3]).

-doc """
Starts the `telemetry` application if it isn't already running.
`Opts` is accepted for API symmetry but currently unused.
""".
-spec start(any()) -> ok.
start(_Opts) ->
    _ = ensure_started(),
    ok.

-doc """
Stops the `telemetry` application, but only if `start/1` was the one that
started it (a host application that already had `telemetry` running keeps it running).
""".
-spec stop() -> ok.
stop() ->
    case ensure_started() of
        internal -> application:stop(telemetry);
        _        -> ok
    end,
    persistent_term:erase(?MODULE),
    ok.

-doc "Forwards the event to `telemetry:execute/3` unchanged.".
-spec event([atom()], map(), map()) -> ok.
event(EventName, Measurements, Metadata) ->
    ensure_started(),
    %% telemetry is an optional dependency; we only call this if the app started successfully
    telemetry:execute(EventName, Measurements, Metadata),
    ok.

%%% Internal

-spec ensure_started() -> internal | external.
ensure_started() ->
    case persistent_term:get(?MODULE, nil) of
        nil ->
            {ok, Apps} = application:ensure_all_started(telemetry),
            Status = case lists:member(telemetry, Apps) of
                true  -> internal;
                false -> external
            end,
            persistent_term:put(?MODULE, Status),
            Status;
        Cached ->
            Cached
    end.
