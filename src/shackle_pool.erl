-module(shackle_pool).
-include("shackle_internal.hrl").

-ignore_xref([
    {shackle_pool_foil, lookup, 1}
]).

%% public
-export([
    start/3,
    start/4,
    stop/1
]).

-export([
    worker_down/2,
    worker_up/2
]).
%% internal
-export([
    init/0,
    server/1,
    terminate/0
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
        {ok, #pool_options{pool_size = PoolSize} = OptionsRec} ->
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
    init_ets(),
    foil:new(?MODULE),
    foil:load(?MODULE).

-spec server(pool_name()) ->
    {ok, client(), atom()} | {error, atom()}.

server(Name) ->
    case options(Name) of
        {ok, #pool_options{
            backlog_size = BacklogSize,
            client = Client,
            pool_size = PoolSize,
            pool_strategy = PoolStrategy
        }} ->
            
            case server_index(Name, PoolSize, PoolStrategy) of
                {error, Reason} ->
                    {error, Reason};
                ServerIndex ->
                    Key = {Name, ServerIndex},
                    {ok, Server} = shackle_pool_foil:lookup(Key),
                    case shackle_backlog:check(Server, BacklogSize) of
                        true ->
                            {ok, Client, Server};
                        false ->
                            {error, backlog_full}
                    end
            end;
        {error, Reson} ->
            {error, Reson}
    end.

-spec terminate() ->
    ok.

terminate() ->
    foil:delete(?MODULE).

-spec worker_down(pool_name(), pos_integer()) -> no_return().
worker_down(PoolName, Index) ->
    ets:insert(?ETS_TABLE_POOL_BAD_WORKERS, {{PoolName, Index}, true}),
    worker_down_cb(ets:update_counter(?ETS_TABLE_POOL_BAD_WORKER_NUMBERS, PoolName, 1), PoolName, options(PoolName)).

-spec worker_up(pool_name(), pos_integer()) -> no_return().
worker_up(PoolName, Index) ->
    ets:delete_object(?ETS_TABLE_POOL_BAD_WORKERS, {PoolName, Index}),
    worker_up_cb(ets:update_counter(?ETS_TABLE_POOL_BAD_WORKER_NUMBERS, PoolName, 1), PoolName, options(PoolName)).

%% private
cleanup(Name, OptionsRec) ->
    cleanup_ets(Name, OptionsRec),
    cleanup_foil(Name, OptionsRec).

cleanup_ets(Name, #pool_options{pool_strategy = round_robin}) ->
    ets:delete(?ETS_TABLE_POOL_INDEX, {Name, round_robin});
cleanup_ets(_Name, _OptionsRec) ->
    ok.

cleanup_foil(Name, #pool_options{pool_size = PoolSize}) ->
    foil:delete(?MODULE, Name),
    [foil:delete(?MODULE, {Name, N}) || N <- lists:seq(1, PoolSize)],
    foil:load(?MODULE).

options(Name) ->
    try shackle_pool_foil:lookup(Name) of
        {ok, Options} ->
            {ok, Options};
        {error, key_not_found} ->
            {error, pool_not_started}
    catch
        error:undef ->
            {error, shackle_not_started}
    end.

options_rec(Client, Options) ->
    BacklogSize = ?LOOKUP(backlog_size, Options, ?DEFAULT_BACKLOG_SIZE),
    PoolSize = ?LOOKUP(pool_size, Options, ?DEFAULT_POOL_SIZE),
    PoolStrategy = ?LOOKUP(pool_strategy, Options, ?DEFAULT_POOL_STRATEGY),
    
    #pool_options{
        backlog_size = BacklogSize,
        client = Client,
        pool_size = PoolSize,
        pool_strategy = PoolStrategy
    }.

server_index(Name, PoolSize, Stratege) ->
    server_index(Name, PoolSize, Stratege, 1).
server_index(Name, PoolSize, _Stratege, N) when N > (0.5 * PoolSize) ->
    %% too many retires and cannot find a live worker. something is wrong with this worker pool
    %% consider disabling it.
    shackle_utils:warning_msg(Name, "Cannot find a live worker after many retries, consider to disable this pool, tried ~p times. ", [N]),
    {error, no_worker};

server_index(Name, PoolSize, random, N) ->
    ServerId = shackle_utils:random(PoolSize),
    check_server_id(Name, PoolSize, ServerId, random, N + 1);
server_index(Name, PoolSize, round_robin, N) ->
    UpdateOps = [{2, 1, PoolSize, 1}],
    Key = {Name, round_robin},
    [ServerId] = ets:update_counter(?ETS_TABLE_POOL_INDEX, Key, UpdateOps),
    check_server_id(Name, PoolSize, ServerId, round_robin, N + 1).

check_server_id(Name, PoolSize, ServerId, Stretegy, N) ->
    case is_worker_disabled(Name, ServerId) of
        true ->
            shackle_utils:warning_msg(Name, "got a dead worker id ~p, try again", [ServerId]),
            server_index(Name, PoolSize, Stretegy, N);
        _ ->
            ServerId
    end.

setup(Name, OptionsRec) ->
    setup_ets(Name, OptionsRec),
    setup_foil(Name, OptionsRec).

setup_ets(Name, #pool_options{pool_strategy = round_robin}) ->
    ets:insert_new(?ETS_TABLE_POOL_INDEX, {{Name, round_robin}, 1}),
    ets:insert_new(?ETS_TABLE_POOL_BAD_WORKER_NUMBERS, {Name, 0});
setup_ets(Name, _OptionsRec) ->
    ets:insert_new(?ETS_TABLE_POOL_BAD_WORKER_NUMBERS, {Name, 0}).

setup_foil(Name, #pool_options{pool_size = PoolSize} = OptionsRec) ->
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

server_spec(ServerMod, ServerName, Name, Client, ClientOptions, Index) ->
    StartFunc = {ServerMod, start_link,
        [ServerName, Name, Client, ClientOptions, Index]},
    {ServerName, StartFunc, permanent, 5000, worker, [ServerMod]}.

start_children(Name, Client, ClientOptions, #pool_options{
    pool_size = PoolSize
}) ->
    
    Protocol = ?LOOKUP(protocol, ClientOptions, ?DEFAULT_PROTOCOL),
    ServerMod = server_mod(Protocol),
    ServerSpecs = [
        begin
            ServerName = server_name(Name, N),
            server_spec(ServerMod, ServerName, Name,
                Client, ClientOptions, N)
        end || N <- lists:seq(1, PoolSize)],
    [supervisor:start_child(?SUPERVISOR, ServerSpec) ||
        ServerSpec <- ServerSpecs].

stop_children([]) ->
    ok;
stop_children([ServerName | T]) ->
    supervisor:terminate_child(?SUPERVISOR, ServerName),
    supervisor:delete_child(?SUPERVISOR, ServerName),
    stop_children(T).

is_worker_disabled(PoolName, Index) ->
    ets:lookup(?ETS_TABLE_POOL_BAD_WORKERS, {PoolName, Index}) /= [].

init_ets() ->
    [
        ets:new(Tab, [
            named_table,
            public,
            {write_concurrency, true}
        ])
        || Tab <- [?ETS_TABLE_POOL_INDEX, ?ETS_TABLE_POOL_BAD_WORKERS, ?ETS_TABLE_POOL_BAD_WORKER_NUMBERS]
    ].

worker_down_cb(NumberOfFailedWorkers, PoolName,
    {ok, #pool_options{pool_size = PoolSize,
    pool_failure_threshold_percentage = DisablePercentage,
    pool_failure_callback_fn = Fun}}) when (NumberOfFailedWorkers / PoolSize) >= DisablePercentage,
                                            is_function(Fun) ->
    Fun(PoolName, NumberOfFailedWorkers);
worker_down_cb(_, _, _)->
    ok.

worker_up_cb(NumberOfFailedWorkersss, PoolName,
    {ok, #pool_options{pool_size = PoolSize,
        pool_recover_threshold_percentage = EnablePercentage,
        pool_recover_callback_fn = Fun}}) when (NumberOfFailedWorkersss / PoolSize) =< EnablePercentage,
    is_function(Fun) ->
    Fun(NumberOfFailedWorkersss, PoolName);
worker_up_cb(_, _, _) ->
    ok.
