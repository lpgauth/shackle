-module(shackle_pool).
-include("shackle.hrl").

%% public
-export([
    start/3,
    stop/1
]).

%% internal
-export([
    init/0,
    server/1
]).

%% public
-spec start(atom(), module(), pool_opts()) -> [{ok, pid()} | {error, atom()}].

start(Name, Client, PoolOpts) ->
    BacklogSize = ?LOOKUP(backlog_size, PoolOpts, ?DEFAULT_BACKLOG_SIZE),
    PoolSize = ?LOOKUP(pool_size, PoolOpts, ?DEFAULT_POOL_SIZE),
    PoolStrategy = ?LOOKUP(pool_strategy, PoolOpts, ?DEFAULT_POOL_STRATEGY),

    setup(Name, #pool_opts {
        backlog_size = BacklogSize,
        client = Client,
        pool_size = PoolSize,
        pool_strategy = PoolStrategy
    }),

    ChildNames = child_names(Client, PoolSize),
    ChildSpecs = [child_spec(Name, ChildName, Client) || ChildName <- ChildNames],

    [supervisor:start_child(?SUPERVISOR, ChildSpec) || ChildSpec <- ChildSpecs].

-spec stop(atom()) -> [ok | {error, atom()}].

stop(Name) ->
    #pool_opts {
        client = Client,
        pool_size = PoolSize,
        pool_strategy = PoolStrategy
    } = opts(Name),

    ChildNames = child_names(Client, PoolSize),
    Results = lists:map(fun (ChildName) ->
    	case supervisor:terminate_child(?SUPERVISOR, ChildName) of
    		ok ->
                supervisor:delete_child(?SUPERVISOR, ChildName);
    		{error, Reason} ->
    			{error, Reason}
    	end
    end, ChildNames),
    case lists:all(fun (Result) -> Result =:= ok end, Results) of
        true -> cleanup(Name, PoolStrategy);
        false -> ok
    end,
    Results.

%% internal
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
        random -> random(PoolSize);
        round_robin -> round_robin(Name, PoolSize)
    end,
    Server = child_name(Client, ServerId),
    case shackle_backlog:check(Server, BacklogSize) of
        true -> {ok, Server};
        false -> {error, backlog_full}
    end.

%% private
child_name(Module, N) ->
    list_to_atom(atom_to_list(Module) ++ integer_to_list(N)).

child_names(Module, PoolSize) ->
    [child_name(Module, N) || N <- lists:seq(1, PoolSize)].

child_spec(Name, ChildName, Module) ->
    StartFunc = {shackle_server, start_link, [Name, ChildName, Module]},
    {ChildName, StartFunc, permanent, 5000, worker, [Module]}.

cleanup(Name, round_robin) ->
    ets:delete(?ETS_TABLE_POOL, Name),
    ets:delete(?ETS_TABLE_POOL, {Name, round_robin});
cleanup(Name, _) ->
    ets:delete(?ETS_TABLE_POOL, Name).

opts(Name) ->
    ets:lookup_element(?ETS_TABLE_POOL, Name, 2).

random(PoolSize) ->
    erlang:phash2({os:timestamp(), self()}, PoolSize) + 1.

round_robin(Name, PoolSize) ->
    UpdateOps = [{2, 1, PoolSize, 1}],
    [RR] = ets:update_counter(?ETS_TABLE_POOL, {Name, round_robin}, UpdateOps),
    RR.

setup(Name, #pool_opts {pool_strategy = round_robin} = Opts) ->
    ets:insert(?ETS_TABLE_POOL, {Name, Opts}),
    ets:insert(?ETS_TABLE_POOL, {{Name, round_robin}, 1});
setup(Name, Opts) ->
    ets:insert(?ETS_TABLE_POOL, {Name, Opts}).
