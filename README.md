# shackle

High-Performance Erlang Network Client Framework

![Build Status](https://github.com/lpgauth/shackle/workflows/Erlang%20CI/badge.svg)

#### Requirements

* Erlang 19.0+

#### Features

* Backpressure via backlog (OOM protection)
* Fast pool implementation (random, round_robin)
* Managed timeouts
* Multi-protocol support (SSL / TCP / UDP)
* Performance-optimized
* Request pipelining
* Smart reconnect mechanism (exponential backoff)

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

```erlang
shackle_pool:start(shackle_pool:name(), client(), client_options(), pool_options())
```

##### client_options:

<table width="100%">
  <theader>
    <th>Name</th>
    <th>Type</th>
    <th>Default</th>
    <th>Description</th>
  </theader>
  <tr>
    <td>address</td>
    <td>inet:ip_address() | inet:hostname()</td>
    <td>"127.0.0.1"</td>
    <td>server address (formerly ip)</td>
  </tr>
  <tr>
    <td>port</td>
    <td>inet:port_number()</td>
    <td>undefined</td>
    <td>server port</td>
  </tr>
  <tr>
    <td>protocol</td>
    <td>shackle_tcp | shackle_udp | shackle_ssl</td>
    <td>shackle_tcp</td>
    <td>server protocol</td>
  </tr>
  <tr>
    <td>reconnect</td>
    <td>boolean()</td>
    <td>true</td>
    <td>reconnect closed connections</td>
  </tr>
  <tr>
    <td>reconnect_time_max</td>
    <td>pos_integer() | infinity</td>
    <td>120000</td>
    <td>maximum reconnect time in milliseconds</td>
  </tr>
  <tr>
    <td>reconnect_time_min</td>
    <td>pos_integer()</td>
    <td>1000</td>
    <td>minimum reconnect time in milliseconds</td>
  </tr>
  <tr>
    <td>socket_options</td>
    <td>[gen_tcp:connect_option() | gen_udp:option()]</td>
    <td>[]</td>
    <td>options passed to the socket</td>
  </tr>
</table>

##### pool_options:

<table width="100%">
  <theader>
    <th>Name</th>
    <th>Type</th>
    <th>Default</th>
    <th>Description</th>
  </theader>
  <tr>
    <td>backlog_size</td>
    <td>pos_integer() | infinity</td>
    <td>1024</td>
    <td>maximum number of concurrent requests per connection</td>
  </tr>
  <tr>
    <td>max_retries</td>
    <td>non_neg_integer()</td>
    <td>3</td>
    <td>maximum number of tries to find an active server</td>
  </tr>
  <tr>
    <td>pool_size</td>
    <td>pos_integer()</td>
    <td>16</td>
    <td>number of connections</td>
  </tr>
  <tr>
    <td>pool_strategy</td>
    <td>random | round_robin</td>
    <td>random</td>
    <td>connection selection strategy</td>
  </tr>
</table>

#### Calling / Casting client

```erlang
1> shackle:call(pool_name, {get, <<"test">>}).
{ok, <<"bar">>}

2> {ok, ReqId} = shackle:cast(pool_name, {get, <<"foo">>}, 500).
{ok, {anchor, anchor_client, #Ref<0.0.0.2407>}}

3> shackle:receive_response(ReqId).
{ok, <<"bar">>}
```

## Telemetry
Shackle integrates with the backend-agnostic [telemetry](https://hexdocs.pm/telemetry/) library. See `shackle_telemetry` for the list of telemetry events that shackle can emit.

## Tests

```makefile
make dialyzer
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

<table width="100%">
  <theader>
    <th>Name</th>
    <th>Description</th>
  </theader>
  <tr>
    <td><a href="https://github.com/lpgauth/anchor">anchor</a></td>
    <td>Memcached Client</td>
  </tr>
  <tr>
    <td><a href="https://github.com/vsavkov/aspike-node">aspike-node</a></td>
    <td>Aerospike Client</td>
  </tr>
  <tr>
    <td><a href="https://github.com/lpgauth/buoy">buoy</a></td>
    <td>HTTP 1.1 Client</td>
  </tr>
  <tr>
    <td><a href="https://github.com/lpgauth/flare">flare</a></td>
    <td>Kafka Producer</td>
  </tr>
  <tr>
    <td><a href="https://github.com/lpgauth/marina">marina</a></td>
    <td>Cassandra CQL Client</td>
  </tr>
  <tr>
    <td><a href="https://github.com/lpgauth/statsderl">statsderl</a></td>
    <td>StatsD Client</td>
  </tr>
</table>

## License

```license
The MIT License (MIT)

Copyright (c) 2015-2023 Louis-Philippe Gauthier

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
