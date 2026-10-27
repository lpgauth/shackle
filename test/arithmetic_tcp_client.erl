-module(arithmetic_tcp_client).
-include("test.hrl").
-include_lib("shackle/include/shackle.hrl").

-export([
    add/2,
    multiply/2,
    noop/0,
    modulo/2,
    delayed_echo/1,
    delayed_echo/2,
    delayed_echo/3,
    delayed_echo_cast/2,
    start/0,
    start/1,
    start/2,
    stop/0,
    wait_until_all_available/1
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

-spec wait_until_all_available(non_neg_integer()) -> boolean().
wait_until_all_available(Timeout) ->
    shackle_pool:wait_until_all_available(?POOL_NAME_TCP, Timeout).

-spec add(tiny_int(), tiny_int()) ->
    pos_integer().

add(A, B) ->
    shackle:call(?POOL_NAME_TCP, {add, A, B}, ?TIMEOUT, infinity).

-spec multiply(tiny_int(), tiny_int()) ->
    pos_integer().

multiply(A, B) ->
    shackle:call(?POOL_NAME_TCP, {multiply, A, B}, ?TIMEOUT).

-spec noop() ->
    ok.

noop() ->
    shackle:call(?POOL_NAME_TCP, noop).

-spec modulo(tiny_int(), tiny_int()) ->
    pos_integer() | {error, atom()}.

modulo(A, B) ->
    shackle:call(?POOL_NAME_TCP, {modulo, A, B}, ?TIMEOUT, infinity).

-spec delayed_echo(pos_integer()) -> pos_integer() | {error, atom()}.
delayed_echo(Delay) ->
    delayed_echo(Delay, ?TIMEOUT).

-spec delayed_echo(pos_integer(), pos_integer()) ->
    pos_integer() | {error, atom()}.
delayed_echo(Delay, CliTimeout) ->
    delayed_echo(Delay, CliTimeout, infinity).

-spec delayed_echo(pos_integer(), pos_integer(), pos_integer()) ->
    pos_integer() | {error, atom()}.
delayed_echo(Delay, CliTimeout, RcvTimeout) ->
    shackle:call(?POOL_NAME_TCP, {delayed_echo, Delay}, CliTimeout, RcvTimeout).

-spec delayed_echo_cast(pos_integer(), pos_integer()) ->
    {ok, pos_integer()} | {error, atom()}.
delayed_echo_cast(Delay, CliTimeout) ->
    shackle:cast(?POOL_NAME_TCP, {delayed_echo, Delay}, self(), CliTimeout).

-spec start() ->
    ok | {error, shackle_not_started | pool_already_started}.

start() ->
    start([
        {backlog_size, ?BACKLOG_SIZE},
        {pool_size, 1}
    ]).

-spec start(shackle_pool:options()) ->
    ok | {error, shackle_not_started | pool_already_started}.
start(PoolOptions) ->
    start(PoolOptions, []).

-spec start(shackle_pool:options(), shackle_client:options()) ->
    ok | {error, shackle_not_started | pool_already_started}.
start(PoolOptions, ClientOptions) ->
    shackle_pool:start(?POOL_NAME_TCP, ?CLIENT_TCP, ClientOptions ++ [
        {service_name, "arithmetic"},
        {port, ?PORT},
        {reconnect, true},
        {reconnect_time_min, 1},
        {socket_options, [
            binary,
            {packet, raw}
        ]}
    ], PoolOptions).

-spec stop() ->
    ok | {error, pool_not_started}.

stop() ->
    shackle_pool:stop(?POOL_NAME_TCP).

%% shackle_server callbacks
init(_) ->
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

handle_data(Data, #state {buffer = Buffer} = State) ->
    Data2 = <<Buffer/binary, Data/binary>>,
    {Replies, Buffer2} = arithmetic_protocol:parse_replies(Data2),
    {ok, Replies, State#state {buffer = Buffer2}}.

handle_timeout(RequestId, State) ->
    {ok, {RequestId, {error, timeout_handled}}, State}.

handle_request(noop, State) ->
    Data = arithmetic_protocol:request(0, noop, 0, 0),
    {ok, undefined, Data, State};

handle_request({Operation, A, B}, #state {request_counter = ReqCount} = State) ->
    RequestId = arithmetic_protocol:request_id(ReqCount),
    Data = arithmetic_protocol:request(RequestId, Operation, A, B),
    {ok, RequestId, Data, State#state {request_counter = ReqCount + 1}};

handle_request({Operation, A}, #state {request_counter = ReqCount} = State) ->
    RequestId = arithmetic_protocol:request_id(ReqCount),
    Data = arithmetic_protocol:request(RequestId, Operation, A),
    {ok, RequestId, Data, State#state {request_counter = ReqCount + 1}};

handle_request(Requests, State) when is_list(Requests) ->
    {RequestIds, Datas, State2} = lists:foldl(
        fun (R, {Ids, Ds, S}) when is_tuple(R); is_atom(R) ->
            {ok, Id, D, S2} = handle_request(R, S),
            {[Id|Ids], [D|Ds], S2}
        end,
        {[], [], State},
        Requests),
    RequestIds2 = lists:reverse(RequestIds),
    Datas2 = lists:reverse(Datas),
    Data = list_to_binary(Datas2),
    {ok, RequestIds2, Data, State2}.

terminate(_State) ->
    ok.
