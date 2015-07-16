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
-define(NAME, arithmetic).

-record(state, {
    buffer = <<>>,
    request_counter = 0
}).

%% public
add(A, B) ->
    shackle:call(?NAME, {add, A, B}).

multiply(A, B) ->
    shackle:call(?NAME, {multiply, A, B}).

start() ->
    shackle_pool:start(?NAME, [
        {client, arithmetic_client},
        {pool_size, 4},
        {pool_strategy, round_robin},
        {backlog_size, 512}
    ]).

stop() ->
    shackle_pool:stop(?NAME).

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
    Request = request(RequestId, Operation, A, B),

    {ok, RequestId, Request, #state {
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

request(RequestId, Operation, A, B) ->
    <<RequestId:8/integer, (opcode(Operation)), A:8/integer, B:8/integer>>.

request_id(RequestCounter) ->
    RequestCounter rem ?MAX_REQUEST_ID.
