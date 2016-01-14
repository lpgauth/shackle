# shackle

__Author:__ Louis-Philippe Gauthier.

Non-blocking Erlang network client framework

[![Build Status](https://travis-ci.org/lpgauth/shackle.svg?branch=master)](https://travis-ci.org/lpgauth/shackle)

### Requirements

* Erlang 16.0 +

### Features

* Backpressure via backlog (OOM protection)
* Fast pool implementation (random, round_robin)
* Multi-protocol support (TCP / UDP)
* Performance optimized
* Request pipelining

## How-to

#### Implementing a client

```erlang
-behavior(shackle_client).
-export([
    options/0,
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

-spec options() -> {ok, Options :: client_options()}.

options() ->
    {ok, [
        {port, 123},
        {protocol, shackle_tcp},
        {reconnect, true}
    ]}.

-spec init() -> {ok, State :: term()}.

init() ->
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

#### Client options

<table width="100%">
  <theader>
    <th>Name</th>
    <th>Type</th>
    <th>Default</th>
    <th>Description</th>
  </theader>
  <tr>
    <td>ip</td>
    <td>inet:ip_address() | inet:hostname()</td>
    <td>"127.0.0.1"</td>
    <td>server ip</td>
  </tr>
  <tr>
    <td>port</td>
    <td>inet:port_number()</td>
    <td>undefined</td>
    <td>server port</td>
  </tr>
  <tr>
    <td>protocol</td>
    <td>shackle_tcp | shackle_udp</td>
    <td>tcp</td>
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
    <td>pos_integer()</td>
    <td>timer:minutes(2)</td>
    <td>maximum reconnect time</td>
  </tr>
  <tr>
    <td>reconnect_time_min</td>
    <td>pos_integer()</td>
    <td>timer:seconds(1)</td>
    <td>minimum reconnect time</td>
  </tr>
  <tr>
    <td>socket_options</td>
    <td>[gen_tcp:connect_option() | gen_udp:connect_option()]</td>
    <td>[]</td>
    <td>options passed to the socket</td>
  </tr>
</table>

#### Starting client pool

```erlang
shackle_pool:start(pool_name, client, [{pool_size, 32}]).
```

#### Pool Options

<table width="100%">
  <theader>
    <th>Name</th>
    <th>Type</th>
    <th>Default</th>
    <th>Description</th>
  </theader>
  <tr>
    <td>backlog_size</td>
    <td>pos_integer()</td>
    <td>1024</td>
    <td>maximum number of concurrent requests per connection</td>
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

#### Calling/Casting client

```erlang
1> shackle:call(pool_name, {get, <<"test">>}).
{ok, <<"bar">>}

2> {ok, ReqId} = shackle:cast(pool_name, {get, <<"foo">>}).
{ok, {anchor, anchor_client, #Ref<0.0.0.2407>}}

3> shackle:receive_response(ReqId).
{ok, <<"bar">>}
```

## Tests

```makefile
make dialyzer
make elvis
make eunit
make xref
```

## Clients

<table width="100%">
  <theader>
    <th>Name</th>
    <th>Description</th>
  </theader>
  <tr>
    <td><a href="https://github.com/lpgauth/anchor">anchor</a></td>
    <td>Memcached client</td>
  </tr>
  <tr>
    <td><a href="https://github.com/lpgauth/marina">marina</a></td>
    <td>Cassandra CQL3 client</td>
  </tr>
</table>

## License

```license
The MIT License (MIT)

Copyright (c) 2015 Louis-Philippe Gauthier

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
