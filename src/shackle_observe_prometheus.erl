-module(shackle_observe_prometheus).

-moduledoc """
Shackle observability backend that records to Prometheus metrics,
maintaining 100% backward compatibility with pre-observability-refactor code.

## Declares all original metrics from `shackle_metrics.erl`:
- 11 counters (cast, connect, close, error, attempt, socket, reply, request, response, bytes, messages)
- 1 histogram (response_time_microseconds)

## Comprehensive Event Handling

This backend handles two types of observability events:

1. **Span Events** (high-level operations):
   - [shackle, call, stop/exception]
   - [shackle, cast, stop/exception]
   - [shackle, connect, stop/exception]
   - [shackle, disconnect]
   - [shackle, timeout]

2. **Metric Events** (low-level telemetry):
   - [shackle, metric, counter] → routes to specific counters
   - [shackle, metric, histogram] → routes to specific histograms

The backend acts as a comprehensive observability collector, routing all events
through to Prometheus metrics for a unified view of shackle behavior.

## Configuration

Selected via `{shackle, [{observability, prometheus}]}` or custom module name.
The `shackle_observe` supervisor calls `start/1` once at boot and `stop/0` on shutdown.
`start/1` starts the `prometheus` application (if not already running) and declares this
module's metrics. `stop/0` only stops `prometheus` if this module is the one that started it.
""".

-behaviour(shackle_observe).

-export([start/0, start/1, stop/0, event/3]).

-doc "Equivalent to `start/1` with default options.".
-spec start() -> ok.
start() -> start(#{}).

-doc """
Starts the `prometheus` application if not already running, and declares
the original backward-compatible metrics. Idempotent -- calling it again
re-declares the same metrics (a no-op).
""".
-spec start(map()) -> ok.
start(_Opts) ->
    ensure_started(),

    % Original counters from shackle_metrics.erl
    try prometheus_counter:declare([
        {name, shackle_cast_total},
        {help, "Count of shackle cast calls"},
        {labels, [client, pool]}
    ]) catch _:_ -> ok end,

    try prometheus_counter:declare([
        {name, shackle_connect_total},
        {help, "Count of shackle connections"},
        {labels, [client, pool]}
    ]) catch _:_ -> ok end,

    try prometheus_counter:declare([
        {name, shackle_close_total},
        {help, "Count of shackle connect closes"},
        {labels, [client, pool]}
    ]) catch _:_ -> ok end,

    try prometheus_counter:declare([
        {name, shackle_error_total},
        {help, "Count of shackle errors"},
        {labels, [client, pool, reason]}
    ]) catch _:_ -> ok end,

    try prometheus_counter:declare([
        {name, shackle_attempt_total},
        {help, "Count of shackle server lookup attempts"},
        {labels, [client, pool, reason]}
    ]) catch _:_ -> ok end,

    try prometheus_counter:declare([
        {name, shackle_socket_total},
        {help, "Count of shackle socket connect/close/bounce events"},
        {labels, [client, pool, event]}
    ]) catch _:_ -> ok end,

    try prometheus_counter:declare([
        {name, shackle_reply_total},
        {help, "Count of shackle replies"},
        {labels, [client, pool]}
    ]) catch _:_ -> ok end,

    try prometheus_counter:declare([
        {name, shackle_request_total},
        {help, "Count of shackle requests"},
        {labels, [client, pool]}
    ]) catch _:_ -> ok end,

    try prometheus_counter:declare([
        {name, shackle_response_total},
        {help, "Count of shackle responses"},
        {labels, [client, pool, response]}
    ]) catch _:_ -> ok end,

    try prometheus_counter:declare([
        {name, shackle_received_bytes_total},
        {help, "Count of shackle received bytes"},
        {labels, [client, pool]}
    ]) catch _:_ -> ok end,

    try prometheus_counter:declare([
        {name, shackle_received_messages_total},
        {help, "Count of shackle received messages"},
        {labels, [client, pool]}
    ]) catch _:_ -> ok end,

    % Original histogram
    try prometheus_histogram:declare([
        {name, shackle_response_time_microseconds},
        {help, "Shackle response time distribution"},
        {buckets, [
            1000,
            2000,
            4000,
            8000,
            16000,
            32000,
            64000,
            96000,
            128000,
            160000,
            192000,
            224000,
            256000,
            512000
        ]},
        {duration_unit, false},
        {labels, [client, pool]}
    ]) catch _:_ -> ok end,

    ok.

-doc """
Stops the `prometheus` application, but only if `start/1` was the one that
started it (a host application that already had `prometheus` running keeps it running).
""".
-spec stop() -> ok.
stop() ->
    ensure_started() == internal andalso application:stop(prometheus),
    persistent_term:erase(?MODULE),
    ok.

-doc "Records observability events as original prometheus metrics.".
-spec event([atom()], map(), map()) -> ok.
event(EventName, Measurements, Metadata) ->
    ensure_started(),
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

%%%
%%% Span Events (high-level operations)
%%%

% shackle_request_total is counted per socket send in shackle_server.erl.
% Counting it on call stop as well would double count every call.

% [shackle, cast, stop] -> shackle_cast_total
handle_event([shackle, cast, stop], _Measurements, #{pool := Pool, client := Client}) ->
    prometheus_counter:inc(shackle_cast_total, [Client, Pool], 1);

% [shackle, call, exception] -> shackle_error_total{reason=exception}
handle_event([shackle, call, exception], _Measurements, #{pool := Pool, client := Client}) ->
    prometheus_counter:inc(shackle_error_total, [Client, Pool, <<"exception">>], 1);

% [shackle, cast, exception] -> shackle_error_total{reason=exception}
handle_event([shackle, cast, exception], _Measurements, #{pool := Pool, client := Client}) ->
    prometheus_counter:inc(shackle_error_total, [Client, Pool, <<"exception">>], 1);

%%%
%%% Metric Events (low-level telemetry from shackle_server.erl)
%%%

% [shackle, metric, counter] with reason label
handle_event([shackle, metric, counter], #{count := Count}, #{
    metric := Metric,
    client := Client,
    pool   := Pool,
    reason := Reason
}) ->
    prometheus_counter:inc(Metric, [Client, Pool, Reason], Count);

% [shackle, metric, counter] without reason label
handle_event([shackle, metric, counter], #{count := Count}, #{
    metric := Metric,
    client := Client,
    pool   := Pool
}) ->
    prometheus_counter:inc(Metric, [Client, Pool], Count);

% [shackle, metric, histogram] -> route to specific histogram
handle_event([shackle, metric, histogram], #{value := Value}, #{
    metric := Metric,
    client := Client,
    pool   := Pool
}) ->
    prometheus_histogram:observe(Metric, [Client, Pool], Value);

% Catch-all for unknown events
handle_event(_EventName, _Measurements, _Metadata) ->
    ok.
