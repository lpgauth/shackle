# shackle

__Author:__ Louis-Philippe Gauthier.

Non-blocking Erlang client framework

[![Build Status](https://travis-ci.org/lpgauth/shackle.svg?branch=master)](https://travis-ci.org/lpgauth/shackle)

### Requirements

* Erlang 16.0 +

### Features

* Asynchronous mode
* Backpressure via backlog (OOM protection)
* Fast pool implementation (random or round_robin)
* Performance optimized
* Request pipelining

### Examples
#### Implementing a Shackle client

```erlang
after_connect(Socket, State) ->
    {ok, State};

handle_cast(Cast, State) ->

    ...

    {ok, RequestId, Request, State}.

handle_data(Data, State) ->

    ...

    {ok, Replies, State}.

options() ->
    {ok, [
        {port, 123123},
        {reconnect, true},
        {state, #state {}}
    ]}.

process_timings(_Cast, _Timings) ->
    ok.

terminate(_State) -> ok.
```

## TODO

* documentation

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
