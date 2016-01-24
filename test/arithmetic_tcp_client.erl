-module(arithmetic_tcp_client).
-include("test.hrl").

-export([
    add/2,
    multiply/2,
    start/0,
    stop/0
]).

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
    buffer = <<>>,
    request_counter = 0
}).

-type tiny_int() :: 0..255.

%% public
-spec add(tiny_int(), tiny_int()) ->
    pos_integer().

add(A, B) ->
    shackle:call(?POOL_NAME, {add, A, B}).

-spec multiply(tiny_int(), tiny_int()) ->
    pos_integer().

multiply(A, B) ->
    shackle:call(?POOL_NAME, {multiply, A, B}).

-spec start() ->
    ok | {error, shackle_not_started | pool_already_started}.

start() ->
    shackle_pool:start(?POOL_NAME, ?CLIENT_TCP).

-spec stop() ->
    ok | {error, pool_not_started}.

stop() ->
    shackle_pool:stop(?POOL_NAME).

%% shackle_server callbacks
options() ->
    {ok, [
        {port, ?PORT},
        {reconnect, true},
        {socket_options, [
            binary,
            {packet, raw}
        ]}
    ]}.

init() ->
    {ok, #state {}}.

setup(Socket, State) ->
    case gen_tcp:send(Socket, <<"INIT">>) of
        ok ->
            case gen_tcp:recv(Socket, 0, ?TIMEOUT) of
                {ok, <<"OK">>} ->
                    {ok, State};
                {error, Reason} ->
                    {error, Reason, State}
            end;
        {error, Reason} ->
            {error, Reason, State}
    end.

handle_data(Data, #state {
        buffer = Buffer
    } = State) ->

    Data2 = <<Buffer/binary, Data/binary>>,
    {Replies, Buffer2} = arithmetic_protocol:parse_replies(Data2),

    {ok, Replies, State#state {
        buffer = Buffer2
    }}.

handle_request({Operation, A, B}, #state {
        request_counter = RequestCounter
    } = State) ->

    RequestId = arithmetic_protocol:request_id(RequestCounter),
    Data = arithmetic_protocol:request(RequestId, Operation, A, B),

    {ok, RequestId, Data, State#state {
        request_counter = RequestCounter + 1
    }}.

terminate(_State) ->
    ok.
