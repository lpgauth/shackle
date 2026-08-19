-module(shackle_observe_prometheus).

-moduledoc """
Shackle observability backend that records straight into Prometheus metrics.

Selected via `{shackle, [{observability, prometheus}]}` or
`{shackle, [{observability, {prometheus, Opts}}]}` where `Opts` is a map
that may contain:

* `buckets => prometheus_histogram:buckets()` -- override the default
  histogram bucket boundaries for all histograms declared by this module.

The `shackle_observe` supervisor calls `start/1` once at boot (after
configuring whatever exporter you use, e.g. `prometheus_httpd`) and
`stop/0` on shutdown. `start/1` both starts the `prometheus` application
(if it isn't running already, the same lazy pattern other optional
backends use) and declares this module's metrics -- do this before
traffic starts flowing to avoid races on metric registration. `stop/0`
only stops `prometheus` if this module is the one that started it.

## Metrics

Declares the following metrics (all labeled with `pool`):

* `shackle_call_duration_seconds` (histogram; labels: `pool`, `result`)
* `shackle_cast_duration_seconds` (histogram; labels: `pool`, `result`)
* `shackle_connect_duration_seconds` (histogram; labels: `pool`, `result`)
* `shackle_call_errors_total` (counter; labels: `pool`, `reason`)
* `shackle_cast_errors_total` (counter; labels: `pool`, `reason`)
* `shackle_connect_errors_total` (counter; labels: `pool`, `reason`)
* `shackle_disconnect_total` (counter; labels: `pool`, `reason`)
* `shackle_timeout_total` (counter; labels: `pool`)
""".

-behaviour(shackle_observe).

-export([start/0, start/1, stop/0, event/3]).

-doc "Equivalent to `start/1` with default histogram buckets.".
-spec start() -> ok.
start() -> start(#{}).

-doc """
Starts the `prometheus` application if not already running, and declares
this module's metrics. Idempotent -- calling it again re-declares the same
metrics (a no-op).
""".
-spec start(map()) -> ok.
start(Opts) ->
    _ = ensure_started(),
    Buckets = maps:get(buckets, Opts, prometheus_buckets:default()),

    % Histograms (latency measurements)
    try prometheus_histogram:declare([
        {name, shackle_call_duration_seconds},
        {help, "Duration of shackle call requests in seconds"},
        {labels, [pool, result]},
        {buckets, Buckets}
    ]) catch _:_ -> ok end,
    try prometheus_histogram:declare([
        {name, shackle_cast_duration_seconds},
        {help, "Duration of shackle cast requests in seconds"},
        {labels, [pool, result]},
        {buckets, Buckets}
    ]) catch _:_ -> ok end,
    try prometheus_histogram:declare([
        {name, shackle_connect_duration_seconds},
        {help, "Duration of shackle connection attempts in seconds"},
        {labels, [pool, result]},
        {buckets, Buckets}
    ]) catch _:_ -> ok end,

    % Error counters
    try prometheus_counter:declare([
        {name, shackle_call_errors_total},
        {help, "Total number of shackle call errors"},
        {labels, [pool, reason]}
    ]) catch _:_ -> ok end,
    try prometheus_counter:declare([
        {name, shackle_cast_errors_total},
        {help, "Total number of shackle cast errors"},
        {labels, [pool, reason]}
    ]) catch _:_ -> ok end,
    try prometheus_counter:declare([
        {name, shackle_connect_errors_total},
        {help, "Total number of shackle connection errors"},
        {labels, [pool, reason]}
    ]) catch _:_ -> ok end,

    % Connection lifecycle counters
    try prometheus_counter:declare([
        {name, shackle_disconnect_total},
        {help, "Total number of shackle disconnects"},
        {labels, [pool, reason]}
    ]) catch _:_ -> ok end,
    try prometheus_counter:declare([
        {name, shackle_timeout_total},
        {help, "Total number of shackle request timeouts"},
        {labels, [pool]}
    ]) catch _:_ -> ok end,

    ok.

-doc """
Stops the `prometheus` application, but only if `start/1` was the one that
started it (a host application that already had `prometheus` running keeps it running).
""".
-spec stop() -> ok.
stop() ->
    case ensure_started() of
        internal -> application:stop(prometheus);
        _        -> ok
    end,
    persistent_term:erase(?MODULE),
    ok.

-doc "Records the event as appropriate Prometheus metric observations.".
-spec event([atom()], map(), map()) -> ok.
event(EventName, Measurements, Metadata) ->
    _ = ensure_started(),
    handle_event(EventName, Measurements, Metadata),
    ok.

%%% Internal

-spec ensure_started() -> internal | external.
ensure_started() ->
    case persistent_term:get(?MODULE, nil) of
        nil ->
            {ok, Apps} = application:ensure_all_started(prometheus),
            Status = case lists:member(prometheus, Apps) of
                true  -> internal;
                false -> external
            end,
            persistent_term:put(?MODULE, Status),
            Status;
        Cached ->
            Cached
    end.

-spec handle_event([atom()], map(), map()) -> ok.

% [shackle, call, stop]
handle_event([shackle, call, stop], #{duration := Duration}, #{pool := Pool, result := Result}) ->
    DurationSeconds = erlang:convert_time_unit(Duration, native, second),
    prometheus_histogram:observe(
        shackle_call_duration_seconds,
        [Pool, atom_to_list(Result)],
        DurationSeconds
    );

% [shackle, call, exception]
handle_event([shackle, call, exception], _Measurements, #{pool := Pool}) ->
    prometheus_counter:inc(shackle_call_errors_total, [Pool, <<"exception">>], 1);

% [shackle, cast, stop]
handle_event([shackle, cast, stop], #{duration := Duration}, #{pool := Pool, result := Result}) ->
    DurationSeconds = erlang:convert_time_unit(Duration, native, second),
    prometheus_histogram:observe(
        shackle_cast_duration_seconds,
        [Pool, atom_to_list(Result)],
        DurationSeconds
    );

% [shackle, cast, exception]
handle_event([shackle, cast, exception], _Measurements, #{pool := Pool}) ->
    prometheus_counter:inc(shackle_cast_errors_total, [Pool, <<"exception">>], 1);

% [shackle, connect, stop]
handle_event([shackle, connect, stop], #{duration := Duration}, #{pool := Pool, result := Result}) ->
    DurationSeconds = erlang:convert_time_unit(Duration, native, second),
    prometheus_histogram:observe(
        shackle_connect_duration_seconds,
        [Pool, atom_to_list(Result)],
        DurationSeconds
    );

% [shackle, connect, exception]
handle_event([shackle, connect, exception], _Measurements, #{pool := Pool}) ->
    prometheus_counter:inc(shackle_connect_errors_total, [Pool, <<"exception">>], 1);

% [shackle, disconnect]
handle_event([shackle, disconnect], _Measurements, #{pool := Pool, reason := Reason}) ->
    ReasonStr = format_reason(Reason),
    prometheus_counter:inc(shackle_disconnect_total, [Pool, ReasonStr], 1);

% [shackle, timeout]
handle_event([shackle, timeout], _Measurements, #{pool := Pool}) ->
    prometheus_counter:inc(shackle_timeout_total, [Pool], 1);

% Catch-all for unknown events
handle_event(_EventName, _Measurements, _Metadata) ->
    ok.

-spec format_reason(term()) -> string().
format_reason(Reason) when is_atom(Reason) ->
    atom_to_list(Reason);
format_reason(Reason) when is_binary(Reason) ->
    binary_to_list(Reason);
format_reason(Reason) ->
    lists:flatten(io_lib:format("~p", [Reason])).
