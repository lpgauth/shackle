%% TODO: parse transform to generate names

-module(shackle_pool).
-include("shackle_internal.hrl").

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
-spec start(pool_name(), client()) -> ok |
    {error, shackle_not_started | pool_already_started}.

start(Name, Client) ->
    start(Name, Client, []).

-spec start(pool_name(), client(), pool_options()) -> ok |
    {error, shackle_not_started | pool_already_started}.

start(Name, Client, PoolOptions) ->
    case options(Name) of
        {ok, _PoolOptions} ->
            {error, pool_already_started};
        {error, shackle_not_started} ->
            {error, shackle_not_started};
        {error, pool_not_started} ->
            PoolOptionsRec = options_rec(Client, PoolOptions),
            setup(Name, PoolOptionsRec),
            start_children(Name, Client, PoolOptionsRec),
            ok
    end.

-spec stop(pool_name()) -> ok |
    {error, shackle_not_started | pool_not_started}.

stop(Name) ->
    case options(Name) of
        {ok, Options} ->
            #pool_options {
                client = Client,
                pool_size = PoolSize,
                pool_strategy = PoolStrategy
            } = Options,

            ManagerNames = manager_names(Client, PoolSize),
            ServerNames = server_names(Client, PoolSize),
            ChildNames = ManagerNames ++ ServerNames,

            lists:foreach(fun (ChildName) ->
                supervisor:terminate_child(?SUPERVISOR, ChildName),
                supervisor:delete_child(?SUPERVISOR, ChildName)
            end, ChildNames),

            cleanup(Name, PoolStrategy),
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

%% internal
-spec init() -> ?ETS_TABLE_POOL.

init() ->
    ets:new(?ETS_TABLE_POOL, [
        named_table,
        public,
        {read_concurrency, true}
    ]).

-spec server(pool_name()) ->
    {ok, client(), server_name(), manager_name()} |
    {error, atom()}.

server(Name) ->
    case options(Name) of
        {ok, Options} ->
            #pool_options {
                backlog_size = BacklogSize,
                client = Client,
                pool_size = PoolSize,
                pool_strategy = PoolStrategy
            } = Options,

            Index = index(Name, PoolSize, PoolStrategy),
            Server = server_name(Client, Index),
            case shackle_backlog:check(Server, BacklogSize) of
                true ->
                    Manager = manager_name(Client, Index),
                    {ok, Client, Server, Manager};
                false ->
                    {error, backlog_full}
            end;
        {error, Reson} ->
            {error, Reson}
    end.

%% private
cleanup(Name, round_robin) ->
    ets:delete(?ETS_TABLE_POOL, Name),
    ets:delete(?ETS_TABLE_POOL, {Name, round_robin});
cleanup(Name, _) ->
    ets:delete(?ETS_TABLE_POOL, Name).

index(_Name, PoolSize, random) ->
    shackle_utils:random(PoolSize) + 1;
index(Name, PoolSize, round_robin) ->
    UpdateOps = [{2, 1, PoolSize, 1}],
    Key = {Name, round_robin},
    [Index] = ets:update_counter(?ETS_TABLE_POOL, Key, UpdateOps),
    Index.

manager_name(Client, N) ->
    Name = [atom_to_list(Client), "_manager_", integer_to_list(N)],
    list_to_atom(lists:flatten(Name)).

manager_names(Client, PoolSize) ->
    [manager_name(Client, N) || N <- lists:seq(1, PoolSize)].

manager_spec(ChildName, Client) ->
    StartFunc = {?MANAGER, start_link, [ChildName, Client]},
    {ChildName, StartFunc, permanent, 5000, worker, [?MANAGER]}.

options(Name) ->
    try
        Options = ets:lookup_element(?ETS_TABLE_POOL, Name, 2),
        {ok, Options}
    catch
        error:badarg ->
            case ets:info(?ETS_TABLE_POOL) of
                undefined ->
                    {error, shackle_not_started};
                _ ->
                    {error, pool_not_started}
            end
    end.

options_rec(Client, PoolOptions) ->
    BacklogSize = ?LOOKUP(backlog_size, PoolOptions, ?DEFAULT_BACKLOG_SIZE),
    PoolSize = ?LOOKUP(pool_size, PoolOptions, ?DEFAULT_POOL_SIZE),
    PoolStrategy = ?LOOKUP(pool_strategy, PoolOptions, ?DEFAULT_POOL_STRATEGY),

    #pool_options {
        backlog_size = BacklogSize,
        client = Client,
        pool_size = PoolSize,
        pool_strategy = PoolStrategy
    }.

server_name(Client, N) ->
    Name = [atom_to_list(Client), "_server_", integer_to_list(N)],
    list_to_atom(lists:flatten(Name)).

server_names(Client, PoolSize) ->
    [server_name(Client, N) || N <- lists:seq(1, PoolSize)].

server_spec(ChildName, Name, Client) ->
    StartFunc = {?SERVER, start_link, [ChildName, Name, Client]},
    {ChildName, StartFunc, permanent, 5000, worker, [?SERVER]}.

setup(Name, #pool_options {pool_strategy = round_robin} = Options) ->
    ets:insert(?ETS_TABLE_POOL, {Name, Options}),
    ets:insert(?ETS_TABLE_POOL, {{Name, round_robin}, 1});
setup(Name, Options) ->
    ets:insert(?ETS_TABLE_POOL, {Name, Options}).

start_children(Name, Client, #pool_options {pool_size = PoolSize}) ->
    ManagerNames = manager_names(Client, PoolSize),
    ManagerSpecs = [manager_spec(ManagerName, Client) ||
        ManagerName <- ManagerNames],

    ServerNames = server_names(Client, PoolSize),
    ServerSpecs = [server_spec(ServerName, Name, Client) ||
        ServerName <- ServerNames],

    ChildSpecs = ManagerSpecs ++ ServerSpecs,
    [supervisor:start_child(?SUPERVISOR, ChildSpec) || ChildSpec <- ChildSpecs].
