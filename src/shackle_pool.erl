-module(shackle_pool).
-include("shackle.hrl").

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

%% public
-spec start(atom(), module()) -> [{ok, pid()} | {error, atom()}].

start(Name, Client) ->
    start(Name, Client, []).

-spec start(atom(), module(), pool_opts()) -> [{ok, pid()} | {error, atom()}].

% TODO: ok | {error, shackle_not_started | pool_already_started}
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
    ChildSpecs = [child_spec(ChildName, Name, Client) || ChildName <- ChildNames],

    [supervisor:start_child(?SUPERVISOR, ChildSpec) || ChildSpec <- ChildSpecs].

-spec stop(atom()) -> ok | {error, pool_not_started}.

stop(Name) ->
    case opts(Name) of
        {ok, Opts} ->
            #pool_opts {
                client = Client,
                pool_size = PoolSize,
                pool_strategy = PoolStrategy
            } = Opts,

            ChildNames = child_names(Client, PoolSize),
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

-spec server(atom()) -> {ok, pid()} | {error, backlog_full | pool_not_started}.
server(Name) ->
    case opts(Name) of
        {ok, Opts} ->
            #pool_opts {
                backlog_size = BacklogSize,
                client = Client,
                pool_size = PoolSize,
                pool_strategy = PoolStrategy
            } = Opts,

            ServerId = case PoolStrategy of
                random -> random(PoolSize);
                round_robin -> round_robin(Name, PoolSize)
            end,
            Server = child_name(Client, ServerId),
            case shackle_backlog:check(Server, BacklogSize) of
                true -> {ok, Server};
                false -> {error, backlog_full}
            end;
        {error, Reson} ->
            {error, Reson}
    end.

%% private
child_name(Client, N) ->
    list_to_atom(atom_to_list(Client) ++ integer_to_list(N)).

child_names(Client, PoolSize) ->
    [child_name(Client, N) || N <- lists:seq(1, PoolSize)].

child_spec(ChildName, Name, Client) ->
    StartFunc = {?SERVER, start_link, [ChildName, Name, Client]},
    {ChildName, StartFunc, permanent, 5000, worker, [?SERVER]}.

cleanup(Name, round_robin) ->
    ets:delete(?ETS_TABLE_POOL, Name),
    ets:delete(?ETS_TABLE_POOL, {Name, round_robin});
cleanup(Name, _) ->
    ets:delete(?ETS_TABLE_POOL, Name).

opts(Name) ->
    try
        Opts = ets:lookup_element(?ETS_TABLE_POOL, Name, 2),
        {ok, Opts}
    catch
        error:badarg ->
            {error, pool_not_started}
    end.

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
