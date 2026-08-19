# Shackle Observability

Shackle now includes a production-grade, pluggable observability system inspired by [arterial](https://github.com/saleyn/arterial)'s proven architecture. The system enables comprehensive monitoring of connection pools, request lifecycles, and network events with **zero overhead when disabled**.

## Features

- **Pluggable backends**: Prometheus, Telemetry, or custom implementations
- **Zero-cost when disabled**: No closures, no branching, no function calls
- **Multiple instrumentation points**: Spans for timed operations (start/stop/exception events), one-shot events for discrete incidents
- **Clean event structure**: Structured metadata and measurements on every event
- **Graceful degradation**: Disabled observability or backend failures don't crash the app
- **Lazy dependency loading**: Only required backends are started

## Quick Start

### Disable Observability (Default)

```erlang
% This is the default; no configuration needed
application:set_env(shackle, observability, nil).
```

### Enable Telemetry Backend

Forward events to the standard Erlang telemetry library for integration with any handler (OpenTelemetry, custom metrics, logging, etc.):

```erlang
application:set_env(shackle, observability, telemetry).

% Attach a handler to capture shackle events
telemetry:attach(my_handler, [shackle, call, stop], fun(EventName, Measurements, Metadata, _Config) ->
    io:format("Event: ~p, Duration: ~p ms~n", [EventName, maps:get(duration, Measurements, 0)])
end, nil).
```

### Enable Prometheus Backend

Record metrics directly into Prometheus for export via HTTP:

```erlang
application:set_env(shackle, observability, prometheus).

% Start shackle app, metrics will be recorded automatically
% Export via prometheus_httpd or prometheus_cowboy2
% Metrics available at /metrics endpoint
```

### Custom Configuration

Pass options to backends:

```erlang
% Telemetry with custom options (reserved for future use)
application:set_env(shackle, observability, {telemetry, #{}}).

% Prometheus with custom histogram buckets
application:set_env(shackle, observability, {prometheus, #{
    buckets => [0.001, 0.01, 0.1, 1.0, 10.0]  % in seconds
}}).

% Custom backend
-module(my_metrics).
-behaviour(shackle_observe).
-export([start/1, stop/0, event/3]).

start(Opts) ->
    % Initialize your metrics system
    ok.

stop() ->
    % Clean up
    ok.

event(EventName, Measurements, Metadata) ->
    % Handle events
    ok.

% Configure it:
application:set_env(shackle, observability, my_metrics).
```

## Event Catalog

All events are prefixed with `[shackle | _]` in the event name.

### Request Lifecycle

- **`[shackle, call, start]`** — Client call begins
  - Metadata: `{pool => PoolName}`
  - Measurements: `{monotonic_time => Time}`

- **`[shackle, call, stop]`** — Client call completes
  - Metadata: `{pool => PoolName, result => ok | error}`
  - Measurements: `{duration => Nanoseconds, monotonic_time => Time}`

- **`[shackle, call, exception]`** — Client call raises exception
  - Metadata: `{pool => PoolName, kind => error|exit|throw, reason => Reason}`
  - Measurements: `{duration => Nanoseconds, monotonic_time => Time}`

- **`[shackle, cast, start|stop|exception]`** — Similar to call, for cast/2,3,4 operations

### Connection Lifecycle

- **`[shackle, connect, start]`** — Connection attempt begins
  - Metadata: `{pool => PoolName}`
  - Measurements: `{monotonic_time => Time}`

- **`[shackle, connect, stop]`** — Connection attempt completes
  - Metadata: `{pool => PoolName, result => ok | error}`
  - Measurements: `{duration => Nanoseconds, monotonic_time => Time}`

- **`[shackle, connect, exception]`** — Connection attempt raises exception
  - Metadata: `{pool => PoolName, kind => ..., reason => ...}`
  - Measurements: `{duration => Nanoseconds, monotonic_time => Time}`

### Error Events

- **`[shackle, timeout]`** — Request timeout
  - Metadata: `{pool => PoolName}`

- **`[shackle, disconnect]`** — Socket disconnected
  - Metadata: `{pool => PoolName, server => ServerName, reason => closed | error | ...}`

- **`[shackle, error]`** — Error on socket or send
  - Metadata: `{pool => PoolName, server => ServerName, reason => Reason}`

## Prometheus Metrics

When using the Prometheus backend, the following metrics are exported:

### Histograms (Request/Connection Latency)

- `shackle_call_duration_seconds` — Time to complete a call/2,3
  - Labels: `pool`, `result` (ok | error)

- `shackle_cast_duration_seconds` — Time to complete a cast/2,3,4
  - Labels: `pool`, `result` (ok | error)

- `shackle_connect_duration_seconds` — Time to establish connection
  - Labels: `pool`, `result` (ok | error)

### Counters (Events)

- `shackle_call_errors_total` — Total call errors
  - Labels: `pool`, `reason`

- `shackle_cast_errors_total` — Total cast errors
  - Labels: `pool`, `reason`

- `shackle_connect_errors_total` — Total connection errors
  - Labels: `pool`, `reason`

- `shackle_disconnect_total` — Total disconnects
  - Labels: `pool`, `reason` (closed | error | ...)

- `shackle_timeout_total` — Total timeouts
  - Labels: `pool`

### Accessing Metrics

```erlang
% Export as text format
prometheus_text_format:format().

% Or via HTTP endpoint (with prometheus_httpd or prometheus_cowboy2)
% GET /metrics
```

## Architecture

The observability system uses a facade pattern with three layers:

### Layer 1: Facade (`shackle_observe`)

Central singleton managing the backend lifecycle. Provides the public API:
- `span/3` — Timed operation wrapping (emits start/stop/exception events)
- `event/2,3` — One-shot event emission
- `enabled/0` — Check if observability is active
- `dispatcher/0` — Get the optimal dispatcher for this context

### Layer 2: Dispatchers

Strategy pattern for eliminating branch overhead:
- `shackle_observe_noop` — Used when disabled (passthrough, zero overhead)
- `shackle_observe_span` — Used when enabled (wraps in spans)

Hot-path code checks the dispatcher once and stores it, calling the same function signatures on both. When disabled, it's literally just calling through to the closure.

### Layer 3: Backends

Implement `behaviour(shackle_observe)` with three callbacks:
- `start/1` — Initialize backend, declare metrics, start dependencies
- `stop/0` — Clean up
- `event/3` — Handle events

Built-in backends:
- `shackle_observe_telemetry` — Forwards to telemetry:execute/3
- `shackle_observe_prometheus` — Records directly to Prometheus

## Performance

When observability is disabled (the default), the system has **zero runtime cost**:

- No closures allocated
- No function calls in hot path (dispatcher pattern + persistent_term caching)
- No branching (persistent_term lookup is a single lookup, cached at init)

Disabled dispatcher functions literally call through to the closure unchanged:

```erlang
call(_PoolName, Fun) ->
    Fun().
```

This means when observability is disabled, `shackle:call/3` has no observability overhead beyond the dispatcher lookup at pool initialization.

## Integration Points

Observability events are emitted at these points in shackle:

- **shackle.erl** — `call/3,4` and `cast/3,4` wrapped in dispatcher spans
- **Future: shackle_server.erl** — Connection attempts, timeouts, disconnects
- **Future: shackle_tcp.erl, shackle_ssl.erl, shackle_udp.erl** — Protocol-specific connection events

No other modules need to be aware of observability; it's completely transparent.

## Debugging

### Check if observability is enabled

```erlang
shackle_observe:enabled().  % true | false
```

### Get the active dispatcher

```erlang
Dispatcher = shackle_observe:dispatcher().  % shackle_observe_noop | shackle_observe_span
```

### Check the configured backend

```erlang
application:get_env(shackle, observability, nil).
```

## Related Reading

- [Arterial Observability](https://github.com/saleyn/arterial/blob/master/src/arterial_observe.erl) — The reference implementation this design is based on
- [Telemetry Library](https://hex.pm/packages/telemetry) — Standard Erlang observability library
- [Prometheus Client for Erlang](https://hex.pm/packages/prometheus) — Prometheus metrics
