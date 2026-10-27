# Multi-Server Support

Shackle supports multiple server addresses for automatic failover and high availability. When a connection to one server fails, the client automatically tries the next server in the list.

## Features

- **Automatic failover**: If connection to a server fails, try the next one
- **Round-robin cycling**: Servers are tried in order, cycling back to the first after reaching the end
- **Per-server ports**: Each server can have its own port number
- **Backwards compatible**: Existing single-server configurations continue to work unchanged
- **Flexible configuration**: Supports bare hostnames, per-server ports, and mixed formats

## Configuration

### Single Server (Backwards Compatible)

```erlang
% Traditional single-server configuration
shackle_pool:start(my_pool, my_client, [
    {address, "server.example.com"},
    {port, 9000}
]).
```

### Multiple Servers - Shared Port

When all servers use the same port, you can provide a list of hostnames:

```erlang
shackle_pool:start(my_pool, my_client, [
    {port, 9000},
    {servers, [
        "server1.example.com",
        "server2.example.com",
        "server3.example.com"
    ]}
]).
```

### Multiple Servers - Per-Server Ports

For servers on different ports, use tuples:

```erlang
shackle_pool:start(my_pool, my_client, [
    {port, 9000},  % Default port used for bare hostnames
    {servers, [
        "server1.example.com",          % Uses default port 9000
        {"server2.example.com", 9001},  % Overrides with port 9001
        {"server3.example.com", 9002}   % Overrides with port 9002
    ]}
]).
```

### Mixed Configuration

You can mix bare hostnames and per-server ports:

```erlang
shackle_pool:start(my_pool, my_client, [
    {port, 9000},
    {servers, [
        "primary.example.com",         % Uses shared port 9000
        {"backup.example.com", 8000},  % Uses explicit port 8000
        "tertiary.example.com"         % Uses shared port 9000
    ]},
    {pool_size, 4}
]).
```

## Failover Behavior

When a connection attempt fails:

1. The client tries the current server with full retry logic (respecting `max_retries`)
2. If the connection fails after all retries, the client moves to the next server
3. The client cycles through all servers in order
4. After trying all servers once, if all have failed, it applies exponential backoff and retries from the first server

## Configuration Options

All standard shackle options are supported with multi-server configuration:

```erlang
shackle_pool:start(my_pool, my_client, [
    {port, 9000},
    {servers, ["s1.example.com", "s2.example.com", "s3.example.com"]},
    {pool_size, 4},                   % Number of connections per pool
    {pool_strategy, random},          % random | round_robin
    {max_retries, 3},                 % Retries per server attempt
    {reconnect, true},                % Auto-reconnect on disconnect
    {reconnect_time_min, 500},        % Min backoff in ms
    {reconnect_time_max, 120000},     % Max backoff in ms (2 minutes)
    {socket_options, [binary, {packet, raw}]},
    {bounce_interval_secs, infinity}  % Graceful connection recycling
]).
```

## Example: High Availability Setup

```erlang
% Start a highly available pool with 3 replicas
shackle_pool:start(redis_pool, redis_client, [
    {port, 6379},
    {servers, [
        "redis-primary.prod.internal",
        "redis-secondary.prod.internal",
        "redis-tertiary.prod.internal"
    ]},
    {pool_size, 8},
    {pool_strategy, round_robin},
    {reconnect, true},
    {reconnect_time_min, 100},
    {reconnect_time_max, 5000}
]).

% Use the pool
shackle:call(redis_pool, {get, <<"my_key">>}, 5000).
```

## Observability

Multi-server failover is fully integrated with shackle's observability system. You can track:

- Connection attempts to each server
- Failover events
- Server availability changes
- Reconnection attempts

See `OBSERVABILITY.md` for configuration details.

## DNS and Load Balancing

Each time shackle connects to a server, it resolves the hostname to one or more IP addresses via DNS. If a hostname resolves to multiple IPs, shackle randomly selects one for each connection attempt.

This provides a second level of load distribution:

```erlang
% If "server.example.com" resolves to 10.0.0.1, 10.0.0.2, 10.0.0.3:
% Each connection attempt randomly picks one of those IPs
% Plus the multi-server failover means connections spread across multiple hosts
shackle_pool:start(my_pool, my_client, [
    {port, 9000},
    {servers, [
        "primary.example.com",    % Resolves to 3 IPs
        "secondary.example.com"   % Resolves to 3 IPs
    ]},
    {pool_size, 6}  % 6 connections distributed across 6 potential IPs
]).
```

## Implementation Details

### Server State Management

Each connection in the pool maintains its own server list and current position. This is managed by the `shackle_servers` module which tracks:

- Normalized list of servers
- Current server index (for round-robin)
- Failed servers in current cycle

### Configuration Normalization

The `{servers, [...]}` configuration is normalized at pool startup time to a list of `{Host, Port}` tuples. This normalization processes each hostname through DNS resolution and handles the multi-server failover configuration.

### Connection Lifecycle

When a connection is established:

1. Get the current server from `shackle_servers:current()`
2. Attempt DNS resolution via `inet:getaddrs(Host, inet)`
3. If DNS fails or times out, try the next server
4. If TCP/SSL connect fails, try the next server
5. If all servers fail, wait according to backoff policy and retry from the first server
6. On successful connection, reset the failed server list

## Backwards Compatibility

This feature is fully backwards compatible:

- Existing code using `{address, Host}` and `{port, Port}` continues to work unchanged
- No changes required to client implementations
- No performance impact for single-server deployments
- All existing tests pass without modification

## Testing

The multi-server functionality includes comprehensive tests in `test/shackle_servers_test.erl`:

- Server list normalization (bare hostnames, per-server ports, mixed)
- Round-robin cycling through all servers
- Tracking failed servers in current cycle
- Resetting failures after successful connection
- Single and multi-server configurations

Run tests with:

```bash
rebar3 eunit -m shackle_servers_test
```

## Troubleshooting

### Servers not being tried in order

The first server in the list is always tried first. Subsequent failures move to the next server in order.

### All servers marked as unavailable

This happens when all servers fail to connect within the current backoff cycle. The client will wait according to the exponential backoff policy and then retry from the first server.

### DNS resolution failures

If hostname resolution fails for all servers, the connection will fail and the pool will use its standard reconnection logic.

### Connection attempts taking too long

Use `socket_options` to set appropriate timeouts:

```erlang
{socket_options, [{connect_timeout, 1000}]}
```

## Performance Considerations

- Multi-server failover has minimal overhead in the success case (single server try)
- Failover latency is proportional to per-server retry attempts (`max_retries`)
- DNS caching by the system can significantly improve failover speed
- Connection pooling amortizes the failover cost across many requests
