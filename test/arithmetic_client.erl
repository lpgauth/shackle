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
-define(POOL_SIZE, 4).
-define(SERVER, arithmetic_client).

-record(state, {
    buffer = <<>>,
    request_counter = 0
}).

%% public
add(A, B) ->
    shackle:call(?SERVER, {add, A, B}, 1000, ?POOL_SIZE).

multiply(A, B) ->
    shackle:call(?SERVER, {multiply, A, B}, 1000, ?POOL_SIZE).

start() ->
    shackle:start_pool(?SERVER, ?POOL_SIZE).

stop() ->
    shackle:stop_pool(?SERVER, ?POOL_SIZE).

%% shackle_server callbacks
init() ->
    {ok, [
        {ip, "127.0.0.1"},
        {port, ?PORT},
        {reconnect, false},
        {state, #state {}}
    ]}.

after_connect(Socket, State) ->
    {ok, Socket, State}.

handle_cast({Operation, A, B}, #state {
        request_counter = RequestCounter
    }) ->

    RequestId = request_id(RequestCounter),
    Data = <<RequestId:8/integer, (opcode(Operation)), A:8/integer, B:8/integer>>,

    {ok, RequestId, Data, #state {
        request_counter = RequestCounter + 1
    }}.

handle_data(Data, #state {
        buffer = Buffer
    } = State) ->

    Data2 = <<Buffer/binary, Data/binary>>,
    {Replies, Buffer2} = parse_replies(Data2, []),

    {ok, Replies, State#state {
        buffer = Buffer2
    }}.

terminate(_State) -> ok.

%% protocol
opcode(add) -> 1;
opcode(multiply) -> 2.

parse_replies(<<ReqId:8/integer, A:16/integer, Rest/binary>>, Acc) ->
    parse_replies(Rest, [{ReqId, A} | Acc]);
parse_replies(Buffer, Acc) ->
    {Acc, Buffer}.

request_id(RequestCounter) ->
    RequestCounter rem ?MAX_REQUEST_ID.
