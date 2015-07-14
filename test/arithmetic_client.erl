-module(arithmetic_client).
-include("test.hrl").

-export([
    add/2,
    multiply/2,
    start/0,
    stop/0
]).

-behavior(shackle_server).
-export([
    init/0,
    after_connect/2,
    handle_cast/2,
    handle_data/2,
    terminate/1
]).

-define(MAX_REQUEST_ID, 256).
-define(NAMESPACE, arithmetic).
% TODO: bump to 4
-define(POOL_SIZE, 1).

-record(state, {
    request_counter = 0
}).

%% public
add(A, B) ->
    shackle:call(arithmetic_client, {add, A, B}, 1000, ?POOL_SIZE).

multiply(A, B) ->
    shackle:call(arithmetic_client, {multiply, A, B}, 1000, ?POOL_SIZE).

start() ->
    shackle:start_pool(arithmetic_client, ?POOL_SIZE).

stop() ->
    shackle:stop_pool(arithmetic_client, ?POOL_SIZE).

%% shackle_server callbacks
init() ->
    {ok, [
        {ip, "127.0.0.1"},
        {port, ?PORT},
        {reconnect, false},
        {state, #state {}}
    ]}.

after_connect(Socket, State) -> {ok, Socket, State}.

handle_cast({Operation, A, B}, #state {
        request_counter = RequestCounter
    }) ->

    RequestId = RequestCounter rem ?MAX_REQUEST_ID,
    Data = <<RequestId:8/integer, (opcode(Operation)), A:8/integer, B:8/integer>>,

    {ok, RequestId, Data, #state {
        request_counter = RequestCounter + 1
    }}.

handle_data(<<ReqId:8/integer, A:16/integer>>, State) ->
    {ok, [{ReqId, A}], State}.

terminate(_State) -> ok.

opcode(add) -> 1;
opcode(multiply) -> 2.
