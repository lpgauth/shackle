-module(arithmetic_client).
-include("test.hrl").

-export([
    add/2,
    multiply/2,
    start/0
]).

-behavior(supervisor).
-export([
    init/1
]).

-behavior(shackle_server).
-export([
    init/0,
    after_connect/2,
    handle_cast/2,
    handle_data/2,
    terminate/1
]).

-define(NAMESPACE, arithmetic).
-define(POOL_SIZE, 4).
-record(state, {}).

%% public
add(A, B) ->
    shackle:call(arithmetic, {add, A, B}, 1000, ?POOL_SIZE).

multiply(A, B) ->
    shackle:call(arithmetic, {multiply, A, B}, 1000, ?POOL_SIZE).

start() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% supervisor callbacks
init([]) ->
    {ok, {{one_for_one, 5, 10},
        shackle:child_specs(?NAMESPACE, ?POOL_SIZE)
    }}.

%% shackle_server callbacks
init() ->
    {ok, [
        {ip, "127.0.0.1"},
        {port, ?PORT},
        {reconnect, false},
        {state, #state {}}
    ]}.

after_connect(Socket, State) -> {ok, Socket, State}.

handle_cast(_Request, State) -> {ok, <<>>, State}.

handle_data(_Data, State) -> {ok, ok, State}.

terminate(_State) -> ok.
