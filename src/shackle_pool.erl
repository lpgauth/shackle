-module(shackle_pool).
-include("shackle_internal.hrl").

-ignore_xref([{shackle_pool_utils, server_name, 2}]).

%% public
-export([
    start/2,
    start/3,
    stop/1
]).

%% internal
-export([
    init/0,
    server/1
]).

%% records
-record(pool_options, {
    backlog_size  :: backlog_size(),
    client        :: client(),
    pool_size     :: pool_size(),
    pool_strategy :: pool_strategy()
}).

%% public
-spec start(pool_name(), client()) ->
    ok | {error, shackle_not_started | pool_already_started}.

start(Name, Client) ->
    start(Name, Client, []).

-spec start(pool_name(), client(), pool_options()) ->
    ok | {error, shackle_not_started | pool_already_started}.

start(Name, Client, Options) ->
    case options(Name) of
        {ok, _OptionsRec} ->
            {error, pool_already_started};
        {error, shackle_not_started} ->
            {error, shackle_not_started};
        {error, pool_not_started} ->
            OptionsRec = options_rec(Client, Options),
            setup(Name, OptionsRec),
            start_children(Name, Client, OptionsRec),
            ok
    end.

-spec stop(pool_name()) ->
    ok | {error, shackle_not_started | pool_not_started}.

stop(Name) ->
    case options(Name) of
        {ok, #pool_options {
                pool_size = PoolSize
            } = OptionsRec} ->

            ServerNames = server_names(Name, PoolSize),
            lists:foreach(fun (ServerName) ->
                supervisor:terminate_child(?SUPERVISOR, ServerName),
                supervisor:delete_child(?SUPERVISOR, ServerName)
            end, ServerNames),
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
    ok.

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
            Server = shackle_pool_utils:server_name(Name, ServerIndex),
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
    generate_pool_utils().

cleanup_ets(Name, #pool_options {pool_strategy = round_robin}) ->
    ets:delete(?ETS_TABLE_POOL, Name),
    ets:delete(?ETS_TABLE_POOL_INDEX, {Name, round_robin});
cleanup_ets(Name, _) ->
    ets:delete(?ETS_TABLE_POOL, Name).

generate_pool_utils() ->
    Pools = [{Name, PoolSize} ||
        {Name, #pool_options {
            pool_size = PoolSize
        }} <- ets:tab2list(?ETS_TABLE_POOL)],
    shackle_generator:pool_utils(Pools).

options(Name) ->
    try
        {ok, ets:lookup_element(?ETS_TABLE_POOL, Name, 2)}
    catch
        error:badarg ->
            case ets:info(?ETS_TABLE_POOL) of
                undefined ->
                    {error, shackle_not_started};
                _ ->
                    {error, pool_not_started}
            end
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
    shackle_utils:random(PoolSize) + 1;
server_index(Name, PoolSize, round_robin) ->
    UpdateOps = [{2, 1, PoolSize, 1}],
    Key = {Name, round_robin},
    [ServerId] = ets:update_counter(?ETS_TABLE_POOL_INDEX, Key, UpdateOps),
    ServerId.

setup(Name, OptionsRec) ->
    setup_ets(Name, OptionsRec),
    generate_pool_utils().

setup_ets(Name, #pool_options {pool_strategy = round_robin} = OptionsRec) ->
    ets:insert(?ETS_TABLE_POOL, {Name, OptionsRec}),
    ets:insert(?ETS_TABLE_POOL_INDEX, {{Name, round_robin}, 1});
setup_ets(Name, OptionsRec) ->
    ets:insert(?ETS_TABLE_POOL, {Name, OptionsRec}).

server_names(Name, PoolSize) ->
    [shackle_pool_utils:server_name(Name, N) ||
        N <- lists:seq(1, PoolSize)].

server_spec(ServerName, Name, Client) ->
    StartFunc = {?SERVER, start_link, [ServerName, Name, Client]},
    {ServerName, StartFunc, permanent, 5000, worker, [?SERVER]}.

start_children(Name, Client, #pool_options {pool_size = PoolSize}) ->
    ServerNames = server_names(Name, PoolSize),
    ServerSpecs = [server_spec(ServerName, Name, Client) ||
        ServerName <- ServerNames],
    [supervisor:start_child(?SUPERVISOR, ServerSpec) ||
        ServerSpec <- ServerSpecs].
