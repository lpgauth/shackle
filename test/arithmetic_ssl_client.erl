-module(arithmetic_ssl_client).
-include("test.hrl").
-include_lib("shackle/include/shackle.hrl").

-export([
    add/2,
    multiply/2,
    noop/0,
    start/0,
    start/1,
    stop/0
]).

-behavior(shackle_client).
-export([
    init/1,
    setup/2,
    handle_request/2,
    handle_data/2,
    handle_timeout/2,
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
    shackle:call(?POOL_NAME, {add, A, B}, ?TIMEOUT).

-spec multiply(tiny_int(), tiny_int()) ->
    pos_integer().

multiply(A, B) ->
    shackle:call(?POOL_NAME, {multiply, A, B}, ?TIMEOUT).

-spec noop() ->
    ok.

noop() ->
    shackle:call(?POOL_NAME, noop).

-spec start() ->
    ok | {error, shackle_not_started | pool_already_started}.

start() ->
    start([
        {backlog_size, ?BACKLOG_SIZE},
        {pool_size, 1}
    ]).

-spec start(pool_options()) ->
    ok | {error, shackle_not_started | pool_already_started}.

start(PoolOptions) ->
    shackle_pool:start(?POOL_NAME, ?CLIENT_SSL, [
        {port, ?PORT},
        {protocol, shackle_ssl},
        {reconnect, true},
        {reconnect_time_min, 1},
        {socket_options, [
            binary,
            {packet, raw},
            {verify, verify_none}
        ]}
    ], PoolOptions).

-spec stop() ->
    ok | {error, pool_not_started}.

stop() ->
    shackle_pool:stop(?POOL_NAME).

%% shackle_server callbacks
init(_) ->
    {ok, #state {}}.

setup(Socket, State) ->
    case ssl:send(Socket, <<"INIT">>) of
        ok ->
            case ssl:recv(Socket, 0, ?TIMEOUT) of
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

handle_timeout(RequestId, State) ->
    {ok, {RequestId, {error, timeout_handled}}, State}.

handle_request(noop, State) ->
    Data = arithmetic_protocol:request(0, noop, 0, 0),

    {ok, undefined, Data, State};
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
