-module(shackle_pool).
-include("shackle_internal.hrl").

-ignore_xref([
    {shackle_pool_utils, options, 1},
    {shackle_pool_utils, server_name, 2}
]).

%% public
-export([
    start/3,
    start/4,
    stop/1
]).

%% internal
-export([
    init/0,
    server/1
]).

%% public
-spec start(pool_name(), client(), client_options()) ->
    ok | {error, shackle_not_started | pool_already_started}.

start(Name, Client, ClientOptions) ->
    start(Name, Client, ClientOptions, []).

-spec start(pool_name(), client(), client_options(), pool_options()) ->
    ok | {error, shackle_not_started | pool_already_started}.

start(Name, Client, ClientOptions, Options) ->
    case options(Name) of
        {ok, _OptionsRec} ->
            {error, pool_already_started};
        {error, shackle_not_started} ->
            {error, shackle_not_started};
        {error, pool_not_started} ->
            OptionsRec = options_rec(Client, Options),
            setup(Name, OptionsRec),
            start_children(Name, Client, ClientOptions, OptionsRec),
            ok
    end.

-spec stop(pool_name()) ->
    ok | {error, shackle_not_started | pool_not_started}.

stop(Name) ->
    case options(Name) of
        {ok, #pool_options {
                pool_size = PoolSize
            } = OptionsRec} ->

            stop_children(server_names(Name, PoolSize)),
            cleanup(Name, OptionsRec),
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

%% internal
-spec init() ->
    ok.

init() ->
    ets:new(?ETS_TABLE_POOL, [
        named_table,
        public,
        {read_concurrency, true}
    ]),
    ets:new(?ETS_TABLE_POOL_INDEX, [
        named_table,
        public,
        {write_concurrency, true}
    ]),
    compile_pool_utils().

-spec server(pool_name()) ->
    {ok, client(), pid()} | {error, atom()}.

server(Name) ->
    case options(Name) of
        {ok, Options} ->
            choose_server(Name, Options);
        {error, Reason} ->
            {error, Reason}
    end.

%% private
choose_server(Name,
              Opts = #pool_options {
                  pool_size = 1,
                  pool_strategy = two_choice
                }) ->
    choose_server(Name, Opts#pool_options{pool_strategy = random});
choose_server(Name,
              Opts = #pool_options {
                 backlog_size = infinity,
                 pool_strategy = two_choice
                }) ->
    choose_server(Name, Opts#pool_options{pool_strategy = random});
choose_server(Name,
              #pool_options {
                 backlog_size = BacklogSize,
                 client = Client,
                 pool_size = PoolSize,
                 pool_strategy = two_choice
                }) ->
    Rand = shackle_utils:random(PoolSize * (PoolSize - 1)) - 1,
    {A, B} = {(Rand rem PoolSize) + 1, (Rand div PoolSize) + 1},
    {IndexA, IndexB} = case A =< B of
                           true -> {A, B + 1};
                           _ -> {A, B}
                       end,
    {ServerA, ServerB} =
        {shackle_pool_utils:server_name(Name, IndexA),
         shackle_pool_utils:server_name(Name, IndexB)},
    Backlogs =
        {shackle_backlog:increment(ServerA, BacklogSize),
         shackle_backlog:increment(ServerB, BacklogSize)},

    case Backlogs of
        {[BacklogSize, BacklogSize], [BacklogSize, BacklogSize]} ->
            {error, backlog_full};
        {[_, ValA], [_, ValB]} when ValA =< ValB ->
            shackle_backlog:decrement(ServerB),
            {ok, Client, ServerA};
        _ ->
            shackle_backlog:decrement(ServerA),
            {ok, Client, ServerB}
    end;

choose_server(Name,
              #pool_options {
                 backlog_size = BacklogSize,
                 client = Client,
                 pool_size = PoolSize,
                 pool_strategy = PoolStrategy
                }) ->
    ServerIndex = server_index(Name, PoolSize, PoolStrategy),
    Server = shackle_pool_utils:server_name(Name, ServerIndex),
    case shackle_backlog:check(Server, BacklogSize) of
        false ->
            {error, backlog_full};
        _ ->
            {ok, Client, Server}
    end.

cleanup(Name, OptionsRec) ->
    cleanup_ets(Name, OptionsRec),
    compile_pool_utils().

cleanup_ets(Name, #pool_options {pool_strategy = round_robin}) ->
    ets:delete(?ETS_TABLE_POOL, Name),
    ets:delete(?ETS_TABLE_POOL_INDEX, {Name, round_robin});
cleanup_ets(Name, _) ->
    ets:delete(?ETS_TABLE_POOL, Name).

compile_pool_utils() ->
    shackle_compiler:pool_utils(ets:tab2list(?ETS_TABLE_POOL)).

options(Name) ->
    try shackle_pool_utils:options(Name) of
        undefined ->
            {error, pool_not_started};
        Options ->
            {ok, Options}
    catch
        _:_ ->
            {error, shackle_not_started}
    end.

options_rec(Client, Options) ->
    BacklogSize = ?LOOKUP(backlog_size, Options, ?DEFAULT_BACKLOG_SIZE),
    PoolSize = ?LOOKUP(pool_size, Options, ?DEFAULT_POOL_SIZE),
    PoolStrategy = ?LOOKUP(pool_strategy, Options, ?DEFAULT_POOL_STRATEGY),

    #pool_options {
        backlog_size = BacklogSize,
        client = Client,
        pool_size = PoolSize,
        pool_strategy = PoolStrategy
    }.

server_index(Name, 1, random) ->
    1;
server_index(_Name, PoolSize, random) ->
    shackle_utils:random(PoolSize);
server_index(Name, PoolSize, round_robin) ->
    UpdateOps = [{2, 1, PoolSize, 1}],
    Key = {Name, round_robin},
    [ServerId] = ets:update_counter(?ETS_TABLE_POOL_INDEX, Key, UpdateOps),
    ServerId.

setup(Name, OptionsRec) ->
    setup_ets(Name, OptionsRec),
    compile_pool_utils().

setup_ets(Name, #pool_options {
        pool_strategy = round_robin
    } = OptionsRec) ->

    ets:insert_new(?ETS_TABLE_POOL, {Name, OptionsRec}),
    ets:insert_new(?ETS_TABLE_POOL_INDEX, {{Name, round_robin}, 1});
setup_ets(Name, OptionsRec) ->
    ets:insert_new(?ETS_TABLE_POOL, {Name, OptionsRec}).

server_names(Name, PoolSize) ->
    [shackle_pool_utils:server_name(Name, N) ||
        N <- lists:seq(1, PoolSize)].

server_mod(shackle_ssl) ->
    shackle_ssl_server;
server_mod(shackle_tcp) ->
    shackle_tcp_server;
server_mod(shackle_udp) ->
    shackle_udp_server.

server_spec(ServerMod, ServerName, Name, Client, ClientOptions) ->
    StartFunc = {ServerMod, start_link,
        [ServerName, Name, Client, ClientOptions]},
    {ServerName, StartFunc, permanent, 5000, worker, [ServerMod]}.

start_children(Name, Client, ClientOptions, #pool_options {
        pool_size = PoolSize
    }) ->

    Protocol = ?LOOKUP(protocol, ClientOptions, ?DEFAULT_PROTOCOL),
    ServerMod = server_mod(Protocol),
    ServerNames = server_names(Name, PoolSize),
    ServerSpecs = [server_spec(ServerMod, ServerName, Name,
        Client, ClientOptions) || ServerName <- ServerNames],
    [supervisor:start_child(?SUPERVISOR, ServerSpec) ||
        ServerSpec <- ServerSpecs].

stop_children([]) ->
    ok;
stop_children([ServerName | T]) ->
    supervisor:terminate_child(?SUPERVISOR, ServerName),
    supervisor:delete_child(?SUPERVISOR, ServerName),
    stop_children(T).
