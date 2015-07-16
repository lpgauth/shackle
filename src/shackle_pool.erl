-module(shackle_pool).
-include("shackle.hrl").

%% public
-export([
    init/0,
    server/1,
    start/2,
    stop/1
]).

-type pool_opt() :: {backlog_size, pos_integer()} |
                    {client, module()} |
                    {pool_size, pos_integer()} |
                    {pool_strategy, random | round_robin}.

-type pool_opts() :: [pool_opt()].

-record(pool_opts, {
    backlog_size,
    client,
    pool_size,
    pool_strategy
}).

%% public
-spec init() -> ?ETS_TABLE_POOL.

init() ->
    ets:new(?ETS_TABLE_POOL, [
        named_table,
        public,
        {read_concurrency, true}
    ]).

-spec server(atom()) -> {ok, pid()} | {error, backlog_full}.
server(Name) ->
    #pool_opts {
        backlog_size = BacklogSize,
        client = Client,
        pool_size = PoolSize,
        pool_strategy = PoolStrategy
    } = opts(Name),

    ServerId = case PoolStrategy of
        random ->
            random(PoolSize);
        round_robin ->
            round_robin(Name, PoolSize)
    end,
    Server = child_name(Client, ServerId),
    case shackle_backlog:check(Server, BacklogSize) of
        true ->
            {ok, Server};
        false ->
            {error, backlog_full}
    end.

-spec start(module(), pool_opts()) -> [{ok, pid()}].

start(Name, PoolOpts) ->
    BacklogSize = ?LOOKUP(backlog_size, PoolOpts, ?DEFAULT_BACKLOG_SIZE),
    % TODO: validate presence
    Client = ?LOOKUP(client, PoolOpts),
    PoolSize = ?LOOKUP(pool_size, PoolOpts, ?DEFAULT_POOL_SIZE),
    PoolStrategy = ?LOOKUP(pool_strategy, PoolOpts, ?DEFAULT_POOL_STRATEGY),
    ets:insert(?ETS_TABLE_POOL, {{Name, round_robin}, 1}),

    set_opts(Name, #pool_opts {
        backlog_size = BacklogSize,
        client = Client,
        pool_size = PoolSize,
        pool_strategy = PoolStrategy
    }),

    ChildNames = child_names(Client, PoolSize),
    ChildSpecs = [child_spec(Name, ChildName, Client) || ChildName <- ChildNames],

    [supervisor:start_child(?SUPERVISOR, ChildSpec) || ChildSpec <- ChildSpecs].

-spec stop(atom()) -> [ok | {error, atom()}].

% TODO: fix me
stop(Name) ->
    #pool_opts {
        client = Client,
        pool_size = PoolSize
    } = opts(Name),

    ChildNames = child_names(Client, PoolSize),
    [supervisor:delete_child(?SUPERVISOR, ChildName) || ChildName <- ChildNames].

%% private
child_name(Module, N) ->
    list_to_atom(atom_to_list(Module) ++ integer_to_list(N)).

child_names(Module, PoolSize) ->
    [child_name(Module, N) || N <- lists:seq(1, PoolSize)].

child_spec(Name, ChildName, Module) ->
    StartFunc = {shackle_server, start_link, [Name, ChildName, Module]},
    {ChildName, StartFunc, permanent, 5000, worker, [Module]}.

opts(Name) ->
    ets:lookup_element(?ETS_TABLE_POOL, Name, 2).

random(PoolSize) ->
    erlang:phash2({os:timestamp(), self()}, PoolSize) + 1.

round_robin(Name, PoolSize) ->
    [X] = ets:update_counter(?ETS_TABLE_POOL, {Name, round_robin}, [{2, 1, PoolSize, 1}]),
    X.

set_opts(Name, Opts) ->
    ets:insert(?ETS_TABLE_POOL, {Name, Opts}).
