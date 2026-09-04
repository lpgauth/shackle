-module(shackle_pool).

-include("shackle_internal.hrl").
-include("shackle_pool.hrl").

-dialyzer({nowarn_function, options/1}).
-dialyzer({nowarn_function, server/3}).
-ignore_xref([
    {shackle_pool_foil, lookup, 1}
]).

%% public
-export([
    start/3,
    start/4,
    stop/1,
    init_bounce/1,
    finalize_bounce/1,
    wait_until_any_available/2,
    wait_until_all_available/2,
    active_all/1,
    active_any/1,
    status/1,
    server_name/2,
    client/1,
    options/1
]).

%% internal
-export([
    init/0,
    server/1,
    server/2,
    terminate/0
]).

%% records

-export_type([
    name/0,
    pool_options/0,
    options/0,
    pool_size/0
]).

%% public
-spec start(shackle_pool:name(), shackle:client(), shackle_client:options()) ->
    ok | {error, shackle_not_started | pool_not_started | pool_already_started}.
start(Name, Client, ClientOptions) ->
    start(Name, Client, ClientOptions, []).

-spec start(shackle_pool:name(), shackle:client(), shackle_client:options(), options()) ->
    ok | {error, shackle_not_started | pool_not_started | pool_already_started}.
start(Name, Client, ClientOptions, Options) ->
    case options(Name) of
        {ok, _OptionsRec} ->
            {error, pool_already_started};
        {error, shackle_not_started} ->
            {error, shackle_not_started};
        {error, pool_not_started} ->
            OptionsRec = options_rec(Name, Client, Options),
            % Normalize servers configuration
            NormalizedClientOpts = normalize_client_options(ClientOptions),
            setup(OptionsRec),
            start_children(Name, Client, NormalizedClientOpts, OptionsRec),
            ok
    end.

-spec stop(shackle_pool:name()) ->
    ok | {error, shackle_not_started | pool_not_started}.
stop(Name) ->
    case options(Name) of
        {ok, #pool_options{
                pool_size = PoolSize
            } = OptionsRec} ->
            stop_children(Name, lists:seq(1, PoolSize)),
            cleanup(Name, OptionsRec),
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

-doc """
Initialize a bounce request for a single connection in the pool. Only one
connection is allowed to bounce at a time. The synchronization is implemented by
using a semaphore.
""".
-spec init_bounce(shackle_pool:name()) -> boolean().
init_bounce(Name) ->
    Sema = persistent_term:get({bounce_sema, Name}),
    {ok, 1} == sema_nif:acquire(Sema).

-doc """
Finalize a bounce request. This function must be called after `init_bounce/1`
and when a connection bounce got completed.
""".
-spec finalize_bounce(shackle_pool:name()) -> ok.
finalize_bounce(Name) ->
    Sema = persistent_term:get({bounce_sema, Name}),
    sema_nif:release(Sema),
    ok.

-doc "Wait until the given server pool is available to accept requests".
-spec wait_until_any_available(shackle_pool:name(), non_neg_integer()) -> boolean().
wait_until_any_available(Name, Timeout) when is_integer(Timeout) ->
    Now = os:system_time(millisecond),
    wait_until_available2(any, Name, Now, Now + Timeout).

-doc "Wait until the given server pool is available to accept requests".
-spec wait_until_all_available(shackle_pool:name(), non_neg_integer()) -> boolean().
wait_until_all_available(Name, Timeout) when is_integer(Timeout) ->
    Now = os:system_time(millisecond),
    wait_until_available2(all, Name, Now, Now + Timeout).

wait_until_available2(_, _Name, Now, Expiration) when Now >= Expiration ->
    false;
wait_until_available2(Method, Name, _Now, Expiration) ->
    case active(Method, Name) of
        false ->
            timer:sleep(10),
            wait_until_available2(Method, Name, os:system_time(millisecond), Expiration);
        _ ->
            true
    end.

active(any, Name) -> active_any(Name);
active(all, Name) -> active_all(Name).

-doc "Return true if the pool has at least one available connection".
-spec active_any(shackle_pool:name()) -> boolean().
active_any(Name) ->
    case options(Name) of
        {ok, #pool_options{pool_size = PSize}} ->
            lists:any(fun(I) ->
                shackle_status:active(_ServerId = {Name, I})
            end, lists:seq(1, PSize));
        {error, _} ->
            false
    end.

-doc "Return true if all connections in the pool are available".
-spec active_all(shackle_pool:name()) -> boolean().
active_all(Name) ->
    case options(Name) of
        {ok, #pool_options{pool_size = PSize}} ->
            lists:all(fun(I) ->
                shackle_status:active(_ServerId = {Name, I})
            end, lists:seq(1, PSize));
        {error, _} ->
            false
    end.

-doc "Return a list of connection handling server names and their active status".
-spec status(shackle_pool:name()) -> [{atom(), active | inactive}].
status(Name) ->
    case options(Name) of
        {ok, #pool_options{pool_size = PSize}} ->
            F = fun
                (true)  -> active;
                (false) -> inactive
            end,
            [{server_name(Name, I), F(shackle_status:active({Name, I}))}
                || I <- lists:seq(1, PSize)];
        {error, _} ->
            []
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

-spec server(name() | pool_options()) ->
    {ok, shackle:client(), atom(), shackle_sema:sema_ref()} |
    {error, atom()}.
server(Name) when is_atom(Name) ->
    case options(Name) of
        {ok, #pool_options{max_retries = MaxRetries} = Options} ->
            server(1, Options, MaxRetries + 1);
        {error, Reason} ->
            {error, Reason}
    end;
server(#pool_options{max_retries = MaxRetries} = Options) ->
    server(1, Options, MaxRetries + 1).

-spec server(name() | pool_options(), pos_integer()) ->
    {ok, shackle:client(), atom(), shackle_sema:sema_ref()} |
    {error, atom()}.
server(Name, Count) when is_atom(Name) ->
    case options(Name) of
        {ok, #pool_options{max_retries = MaxRetries} = Options} ->
            server(Count, Options, MaxRetries + 1);
        {error, Reason} ->
            {error, Reason}
    end;
server(#pool_options{max_retries = MaxRetries} = Options, Count) ->
    server(Count, Options, MaxRetries + 1).

-spec terminate() -> ok.
terminate() ->
    foil:delete(?MODULE).

%% private
cleanup(Name, OptionsRec) ->
    shackle_sema:delete(Name),
    shackle_status:delete(Name),
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

-spec options(atom()) ->
    {ok, pool_options()} | {error, pool_not_started | shackle_not_started}.
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

-doc "Get the client module for a pool.".
-spec client(atom()) ->
    {ok, shackle:client()} | {error, pool_not_started | shackle_not_started}.
client(Name) ->
    case options(Name) of
        {ok, #pool_options{client = Client}} ->
            {ok, Client};
        {error, Reason} ->
            {error, Reason}
    end.

options_rec(PoolName, Client, Options) ->
    Options1 = shackle_utils:default_options(pool, Options),
    BacklogSize = ?LOOKUP(backlog_size, Options1, ?DEFAULT_BACKLOG_SIZE),
    MaxRetries = ?LOOKUP(max_retries, Options1, ?DEFAULT_MAX_RETRIES),
    PoolSize = ?LOOKUP(pool_size, Options1, ?DEFAULT_POOL_SIZE),
    PoolStrategy = ?LOOKUP(pool_strategy, Options1, ?DEFAULT_POOL_STRATEGY),
    #pool_options {
        name = PoolName,
        backlog_size = BacklogSize,
        client = Client,
        max_retries = MaxRetries,
        pool_size = PoolSize,
        pool_strategy = PoolStrategy
    }.

server(_Count, #pool_options{name = Name, client = Client}, 0) ->
    observe_pool_metric(Client, Name, shackle_error_total, <<"no server">>),
    {error, no_server};
server(
    Count,
    #pool_options{
        name = Name,
        backlog_size = BacklogSize,
        client = Client,
        pool_size = PoolSize,
        pool_strategy = PoolStrategy
    } = Options,
    N
) ->
    ServerId = {Name, ServerIdx} = server_id(Name, PoolSize, PoolStrategy),
    case shackle_status:active(ServerId) of
        true when BacklogSize =:= infinity ->
            {ok, ServerName} = shackle_pool_foil:lookup(ServerId),
            {ok, Client, ServerName, undefined};
        true ->
            case shackle_sema:acquire(Name, ServerIdx, Count) of
                {ok, Sema} ->
                    {ok, ServerName} = shackle_pool_foil:lookup(ServerId),
                    {ok, Client, ServerName, Sema};
                error ->
                    observe_pool_metric(Client, Name, shackle_attempt_total, <<"backlog full">>),
                    server(Count, Options, N - 1)
            end;
        false ->
            observe_pool_metric(Client, Name, shackle_attempt_total, <<"disabled">>),
            server(Count, Options, N - 1)
    end.

-doc "Emit observability event for pool metrics".
observe_pool_metric(Client, Pool, Metric, Reason) ->
    shackle_observe:event([metric, counter], #{count => 1}, #{
        metric => Metric,
        client => Client,
        pool   => Pool,
        reason => Reason
    }).

server_id(Name, PoolSize, random) ->
    {Name, shackle_utils:random(PoolSize)};
server_id(Name, PoolSize, round_robin) ->
    UpdateOps = [{2, 1, PoolSize, 1}],
    Key = {Name, round_robin},
    [ServerId] = ets:update_counter(?ETS_TABLE_POOL_INDEX, Key, UpdateOps),
    {Name, ServerId}.

setup(#pool_options {
        name         = Name,
        backlog_size = BacklogSize,
        pool_size    = PoolSize
    } = OptionsRec) ->
    shackle_sema:new(Name, PoolSize, BacklogSize),
    %% Create a semaphore for this pool name to be used for checking if a
    %% connection can be bounced gracefully. The semaphore is used to permit
    %% only a single connection bounce in a pool at any point in time.
    persistent_term:put({bounce_sema, Name}, sema_nif:create(1)),
    shackle_status:new(Name, PoolSize),
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

-spec server_name(shackle_pool:name(), non_neg_integer()) -> atom().
server_name(Name, Index) ->
    list_to_atom(atom_to_list(Name) ++ "_" ++ integer_to_list(Index)).

server_spec(Name, Index, Client, ClientOptions) ->
    ServerName = server_name(Name, Index),
    ServerOpts = {Name, Index, Client, ClientOptions},
    StartFunc = {?SERVER, start_link, [ServerName, ServerOpts]},
    {ServerName, StartFunc, permanent, 5000, worker, [?SERVER]}.

start_children(Name, Client, ClientOptions, #pool_options {pool_size = PoolSize}) ->
    [supervisor:start_child(?SUPERVISOR,
        server_spec(Name, Index, Client, ClientOptions)) ||
        Index <- lists:seq(1, PoolSize)].

stop_children(_Name, []) ->
    ok;
stop_children(Name, [Index | T]) ->
    ServerName = server_name(Name, Index),
    supervisor:terminate_child(?SUPERVISOR, ServerName),
    supervisor:delete_child(?SUPERVISOR, ServerName),
    stop_children(Name, T).

%%--------------------------------------------------------------------
%% @private
%% Normalize client options to support both single server and multi-server configs.
%%
%% If 'servers' list is provided, it normalizes it using shackle_servers:new/2.
%% If only 'address' and 'port' are provided, converts them to 'servers' list format.
%% @end
normalize_client_options(ClientOptions) ->
    case ?LOOKUP(servers, ClientOptions, undefined) of
        undefined ->
            % No servers list - return as-is for backwards compatibility
            % The validation will happen at shackle_server:init time
            ClientOptions;
        ServersList ->
            % Servers list provided, normalize it
            Port = ?LOOKUP(port, ClientOptions),
            try shackle_servers:new(ServersList, Port) of
                ServersState ->
                    NormalizedServers = maps:get(servers, ServersState),
                    [{servers, NormalizedServers} |
                     lists:keydelete(servers, 1, ClientOptions)]
            catch
                error:Error ->
                    error({invalid_servers_config, Error, ServersList})
            end
    end.
