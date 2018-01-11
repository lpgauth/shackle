-module(shackle_pool).
-include("shackle_internal.hrl").

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
    ets:new(?ETS_TABLE_POOL_INDEX, [
        named_table,
        public,
        {write_concurrency, true}
    ]),
    foil:new(?MODULE),
    foil:load(?MODULE).

-spec server(pool_name()) ->
    {ok, client(), pid()} | {error, atom()}.

server(Name) ->
    case options(Name) of
        {ok, #pool_options {
                backlog_size = BacklogSize,
                client = Client,
                pool_size = PoolSize,
                pool_strategy = PoolStrategy
            }} ->

            ServerIndex = server_index(Name, PoolSize, PoolStrategy),
            {ok, Server} = foil:lookup(?MODULE, {Name, ServerIndex}),
            case shackle_backlog:check(Server, BacklogSize) of
                true ->
                    {ok, Client, Server};
                false ->
                    {error, backlog_full}
            end;
        {error, Reson} ->
            {error, Reson}
    end.

%% private
cleanup(Name, OptionsRec) ->
    cleanup_ets(Name, OptionsRec),
    cleanup_foil(Name, OptionsRec).

cleanup_ets(Name, #pool_options {pool_strategy = round_robin}) ->
    ets:delete(?ETS_TABLE_POOL_INDEX, {Name, round_robin});
cleanup_ets(_Name, _OptionsRec) ->
    ok.

cleanup_foil(Name, #pool_options {pool_size = PoolSize}) ->
    foil:delete(?MODULE, Name),
    [foil:delete(?MODULE, {Name, N}) || N <- lists:seq(1, PoolSize)],
    foil:load(?MODULE).

options(Name) ->
    case foil:lookup(?MODULE, Name) of
        {ok, Options} ->
            {ok, Options};
        {error, key_not_found} ->
            {error, pool_not_started};
        {error, module_not_found} ->
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

server_index(_Name, PoolSize, random) ->
    shackle_utils:random(PoolSize);
server_index(Name, PoolSize, round_robin) ->
    UpdateOps = [{2, 1, PoolSize, 1}],
    Key = {Name, round_robin},
    [ServerId] = ets:update_counter(?ETS_TABLE_POOL_INDEX, Key, UpdateOps),
    ServerId.

setup(Name, OptionsRec) ->
    setup_ets(Name, OptionsRec),
    setup_foil(Name, OptionsRec).

setup_ets(Name, #pool_options {pool_strategy = round_robin}) ->
    ets:insert_new(?ETS_TABLE_POOL_INDEX, {{Name, round_robin}, 1});
setup_ets(_Name, _OptionsRec) ->
    ok.

setup_foil(Name, #pool_options {pool_size = PoolSize} = OptionsRec) ->
    foil:insert(?MODULE, Name, OptionsRec),
    [foil:insert(?MODULE, {Name, N}, server_name(Name, N)) ||
        N <- lists:seq(1, PoolSize)],
    foil:load(?MODULE).

server_name(Name, Index) ->
    list_to_atom(atom_to_list(Name) ++ "_" ++ integer_to_list(Index)).

server_names(Name, PoolSize) ->
    [server_name(Name, N) || N <- lists:seq(1, PoolSize)].

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
