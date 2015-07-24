-module(arithmetic_client).
-include("test.hrl").

-export([
    add/2,
    multiply/2,
    start/0,
    stop/0
]).

-behavior(shackle_client).
-export([
    opts/0,
    after_connect/2,
    handle_cast/2,
    handle_data/2,
    terminate/1
]).

-define(MAX_REQUEST_ID, 256).

-record(state, {
    buffer = <<>>,
    request_counter = 0
}).

-type tiny_int() :: 0..255.

%% public
-spec add(tiny_int(), tiny_int()) -> pos_integer().

add(A, B) ->
    shackle:call(?POOL_NAME, {add, A, B}).

-spec multiply(tiny_int(), tiny_int()) -> pos_integer().

multiply(A, B) ->
    shackle:call(?POOL_NAME, {multiply, A, B}).

-spec start() -> ok | {error, shackle_not_started | pool_already_started}.

start() ->
    shackle_pool:start(?POOL_NAME, ?CLIENT).

-spec stop() -> ok | {error, pool_not_started}.

stop() ->
    shackle_pool:stop(?POOL_NAME).

%% shackle_server callbacks
opts() ->
    {ok, [
        {port, ?PORT},
        {reconnect, true},
        {state, #state {}}
    ]}.

after_connect(Socket, State) ->
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
