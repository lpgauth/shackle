# shackle

![Build Status](https://github.com/lpgauth/shackle/workflows/Erlang%20CI/badge.svg)

This project is an open-source connection connection-pooling library that
implements a high-performance Erlang framework for building scalable network
clients and servers.

#### Requirements

* Erlang 27+

#### Features

* Automatic failover and high availability with multi-server support
* Backpressure via backlog (OOM protection)
* Advanced pool strategies (random, round_robin, power_of_two, least_connections, random_without_replacement, consistent_hash)
* Managed timeouts
* Multi-protocol support (SSL / TCP / UDP)
* Performance-optimized
* Request pipelining
* Smart reconnect mechanism (exponential backoff)
* Defining timeout of an established connection
* Pluggable observability
* Customizable server address resolution (DNS, K8s, caching, etc.)

#### Framework goals

* Reusability
* Speed
* Concurrency
* Safety

## How-to

#### Implementing a client

```erlang
-behavior(shackle_client).
-export([
    init/0,
    setup/2,
    handle_request/2,
    handle_data/2,
    terminate/1
]).

-record(state, {
    buffer =       <<>> :: binary(),
    request_counter = 0 :: non_neg_integer()
}).

-spec init(Options :: term()) ->
    {ok, State :: term()} |
    {error, Reason :: term()}.

init(_Options) ->
    {ok, #state {}}.

-spec setup(Socket :: inet:socket(), State :: term()) ->
    {ok, State :: term()} |
    {error, Reason :: term(), State :: term()}.

setup(Socket, State) ->
    case gen_tcp:send(Socket, <<"INIT">>) of
        ok ->
            case gen_tcp:recv(Socket, 0) of
                {ok, <<"OK">>} ->
                    {ok, State};
                {error, Reason} ->
                    {error, Reason, State}
            end;
        {error, Reason} ->
            {error, Reason, State}
    end.

-spec handle_request(Request :: term(), State :: term()) ->
    {ok, RequestId :: external_request_id(), Data :: iodata(), State :: term()}.

handle_request(noop,  State) ->
    Data = arithmetic_protocol:request(0, noop, 0, 0),

    {ok, undefined, Data, State};
handle_request({Operation, A, B}, #state {
        request_counter = RequestCounter
    } = State) ->

    RequestId = request_id(RequestCounter),
    Data = request(RequestId, Operation, A, B),

    {ok, RequestId, Data, State#state {
        request_counter = RequestCounter + 1
    }}.

-spec handle_data(Data :: binary(), State :: term()) ->
    {ok, [{RequestId :: external_request_id(), Reply :: term()}], State :: term()}.

handle_data(Data, #state {
        buffer = Buffer
    } = State) ->

    Data2 = <<Buffer/binary, Data/binary>>,
    {Replies, Buffer2} = parse_replies(Data2, []),

    {ok, Replies, State#state {
        buffer = Buffer2
    }}.

-spec terminate(State :: term()) -> ok.

terminate(_State) -> ok.
```

#### Starting client pool

By default a shackle client implementation will read the configuration
options from the `shackle` application config:

```erlang
{shackle, [
    {pool,   PoolOptions::pool_options()},
    {client, ClientOptions::client_options()}
]}.
```

The options passed as `client_options()`, `pool_options()` to
`shackle_pool:start/4` will override the defaults:

```erlang
shackle_pool:start(pool_name(), client(), client_options(), pool_options())
```

##### client_options:

| Name | Type | Default | Description |
|------|------|---------|-------------|
| address | `inet:ip_address() \| inet:hostname()` | `"127.0.0.1"` | server address (formerly ip) |
| port | `inet:port_number()` | undefined | server port |
| protocol | `shackle_tcp \| shackle_udp \| shackle_ssl` | shackle_tcp | server protocol |
| reconnect | `boolean()` | true | reconnect closed connections |
| reconnect_time_max | `pos_integer() \| infinity` | 120000 | maximum reconnect time in milliseconds |
| reconnect_time_min | `pos_integer()` | 1000 | minimum reconnect time in milliseconds |
| socket_options | `[gen_tcp:connect_option() \| gen_udp:option()]` | `[]` | options passed to the socket |
| bounce_interval_secs | `pos_integer() \| infinity` | infinity | number of seconds between forcing a connection bounce for every connection in a pool |
| bounce_udp | `boolean()` | false | When connection bouncing is enabled (`bounce_interval_secs` is an integer) UDP connections by default are not bounced unless this option is `true` |
| on_bounce_event | `{Module::atom(), Function::atom()}` | — | Module:Function/3 to call on connection bounce events. The function will receive three arguments: `PoolName::atom()`, `ServerInstance::integer()`, and `Event::map()`. This is useful for troubleshooting and logging. |
| servers | `[string() \| {string(), inet:port_number()}]` | undefined | list of server addresses for multi-server failover (see below) |

##### pool_options:

| Name | Type | Default | Description |
|------|------|---------|-------------|
| backlog_size | `pos_integer() \| infinity` | 1024 | maximum number of concurrent requests per connection |
| max_retries | `non_neg_integer()` | 3 | maximum number of tries to find an active server |
| pool_size | `pos_integer()` | 16 | number of connections |
| pool_strategy | `random \| round_robin \| power_of_two \| least_connections \| random_without_replacement \| consistent_hash` | random | connection selection strategy (see Pool Strategies below) |

#### Pool Strategies

The pool strategy determines how connections are selected when serving requests:

**`random` (default)**
- Selects a random connection from the pool
- Time: O(1), Space: O(1)
- Best for: General-purpose use
- Note: May select same connection multiple times on retries in small pools

```erlang
shackle_pool:start(my_pool, my_client, ClientOpts, [{pool_strategy, random}]).
```

**`round_robin`**
- Cycles through connections in order
- Time: O(1), Space: O(1)
- Best for: Uniform load distribution when request durations are predictable
- Deterministic: guaranteed to distribute requests evenly

```erlang
shackle_pool:start(my_pool, my_client, ClientOpts, [{pool_strategy, round_robin}]).
```

**`power_of_two` (Recommended)**
- Samples 2 random connections and selects the one with fewer active requests
- Time: O(1), Space: O(n) (load tracking)
- Best for: General-purpose workloads with variable request durations
- Advantages: Near-optimal load balancing with minimal computational overhead
- Popular in production systems (Nginx, HAProxy)

```erlang
shackle_pool:start(my_pool, my_client, ClientOpts,
    [{pool_strategy, power_of_two}, {pool_size, 16}]).
```

**`least_connections`**
- Scans all active connections and selects the one with the lowest backlog
- Time: O(n), Space: O(n) (load tracking)
- Best for: Applications requiring strict load balancing
- Advantages: Guarantees optimal load distribution
- Trade-off: Slightly higher CPU cost

```erlang
shackle_pool:start(my_pool, my_client, ClientOpts,
    [{pool_strategy, least_connections}, {pool_size, 8}]).
```

**`random_without_replacement`**
- Generates a random permutation of connections; retries select different connections
- Time: O(n) shuffle + O(1) per selection, Space: O(n)
- Best for: Applications with aggressive retry logic
- Advantages: Guarantees trying all different connections before repeating
- Solves: Random strategy issue where same connection may be tried multiple times

```erlang
shackle_pool:start(my_pool, my_client, ClientOpts,
    [{pool_strategy, random_without_replacement}, {max_retries, 5}]).
```

Example with retries:
```erlang
%% With pool size 4 and max_retries = 3:
%% Permutation: [3, 1, 4, 2]
%% Attempt 1: Try server 3 (fails)
%% Attempt 2: Try server 1 (fails)
%% Attempt 3: Try server 4 (fails)
%% Attempt 4: Try server 2 (succeeds)
%% All 4 different servers tried before repeating
```

**`consistent_hash`**
- Uses deterministic hashing for consistent connection selection
- Time: O(1), Space: O(1)
- Best for: Applications needing predictable routing (e.g., stateful backends)
- Advantages: Same hash input always selects same connection
- Foundation: Enables session affinity via caller-provided hash keys

```erlang
% Start pool with consistent_hash strategy
shackle_pool:start(my_pool, my_client, ClientOpts,
    [{pool_strategy, consistent_hash}, {pool_size, 32}]).

% Route requests by hash key - same key always goes to same server
shackle:call(my_pool, {get, "session123"}, #{hash_key => "session123"}).
shackle:cast(my_pool, {set, "session123", Data}, #{hash_key => "session123", timeout => 1000}).
```

##### Strategy Comparison

| Strategy | Time | Space | Load Balance | Predictable | Use Case |
|----------|------|-------|--------------|-------------|----------|
| random | O(1) | O(1) | Poor | ✗ | Default, simple use |
| round_robin | O(1) | O(1) | Perfect | ✓ | Uniform workloads |
| **power_of_two** | O(1) | O(n) | Excellent | ✗ | **General purpose (recommended)** |
| least_connections | O(n) | O(n) | Perfect | ✓ | Critical applications |
| random_without_replacement | O(n) | O(n) | Good | ✗ | Retry-heavy workloads |
| consistent_hash | O(1) | O(1) | Variable | ✓ | Stateful backends |

#### Multi-server failover

_These features were copied from the [Arterial](https://github.com/saleyn/arterial) project._

Shackle supports multiple server addresses for automatic failover and high availability. When a connection to one server fails, the client automatically tries the next server in the list.

```erlang
%% Multiple servers with shared port
shackle_pool:start(my_pool, my_client, [
    {port, 9000},
    {servers, [
        "server1.example.com",
        "server2.example.com",
        "server3.example.com"
    ]}
], []).

%% Per-server ports
shackle_pool:start(my_pool, my_client, [
    {port, 9000},
    {servers, [
        "server1.example.com",          % Uses default port 9000
        {"server2.example.com", 9001},  % Overrides with port 9001
        {"server3.example.com", 9002}
    ]}
], []).
```

Failover behavior:

1. The client tries the current server with full retry logic (respecting `max_retries`)
2. On failure, moves to the next server in the list
3. After all servers are exhausted, applies exponential backoff and retries from the first server

DNS resolution results are cached (5-minute TTL) and hostnames resolving to multiple IPs get random selection per connection attempt. Kubernetes service names are automatically detected and resolved using cluster DNS search domains.

Existing single-server configurations (`{address, Host}` + `{port, Port}`) continue to work unchanged.

#### Calling / Casting client

```erlang
%% Basic calls and casts
1> shackle:call(pool_name, {get, <<"test">>}).
{ok, <<"bar">>}

2> {ok, ReqId} = shackle:cast(pool_name, {get, <<"foo">>}, self()).
{ok, {anchor, anchor_client, #Ref<0.0.0.2407>}}

3> shackle:receive_response(ReqId).
{ok, <<"bar">>}

%% Using options for timeout and hash key
4> shackle:call(my_pool, {get, <<"key">>}, #{timeout => 1000}).
{ok, <<"value">>}

5> shackle:call(my_pool, {get, <<"key">>}, #{hash_key => "session123"}).
{ok, <<"value">>}

6> shackle:cast(my_pool, {set, <<"key">>, <<"value">>}, #{
     hash_key => "session123",
     timeout => 2000
   }).
{ok, ReqId}

%% With specific destination process
7> shackle:cast(my_pool, {get, <<"test">>}, DestPid, #{timeout => 1000}).
{ok, ReqId}
```

## Observability

Shackle includes a production-grade observability system that replaced the legacy hooks mechanism. For metrics collection and monitoring, use the modern observability backend system.

See [Observability](observability.md) for detailed documentation on:
- Prometheus metrics backend
- Telemetry integration
- Custom backend implementation
- Event catalog and integration points

## Tests

```makefile
make dialyzer
make lint
make eunit
make xref
```

## Performance testing

To run performance testing targets you must first start the server:

```
./bin/server.sh
```

Then you can run the `bench` or `profile` target:

```makefile
make bench
make profile
```

## Clients

| Name | Description |
|------|-------------|
| [anchor](https://github.com/lpgauth/anchor) | Memcached Client |
| [buoy](https://github.com/lpgauth/buoy) | HTTP 1.1 Client |
| [flare](https://github.com/lpgauth/flare) | Kafka Producer |
| [marina](https://github.com/lpgauth/marina) | Cassandra CQL Client |
| [statsderl](https://github.com/lpgauth/statsderl) | StatsD Client |

## Advanced Features

For more advanced features, check out the following documentation:

- [Multi-Server Support](multi-server.md) - Automatic failover and high availability with multiple server addresses
- [DNS Resolution](dns-resolution.md) - Enhanced hostname resolution with caching and Kubernetes service discovery
- [Observability](observability.md) - Monitoring and observability features

## License

```license
The MIT License (MIT)

Copyright (c) 2015-2020 Louis-Philippe Gauthier

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
```
