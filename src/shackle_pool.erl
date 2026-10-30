-module(shackle_pool).

-doc """
Connection pool management for Shackle.

Implements various server selection strategies for load balancing and connection pooling:

## Pool Strategies

- **random**: Selects a random connection (default, O(1))
- **round_robin**: Cycles through connections deterministically (O(1))
- **power_of_two**: Samples 2 connections, picks the less-loaded (O(1), recommended)
- **least_connections**: Scans all connections, picks the minimum-loaded (O(n), optimal)
- **random_without_replacement**: Random permutation; retries never repeat server (O(n))
- **consistent_hash**: Hash-based selection for deterministic routing (O(1))

## Examples

Start a pool with power_of_two strategy (recommended):
```erlang
shackle_pool:start(my_pool, my_client, ClientOpts,
    [{pool_strategy, power_of_two}, {pool_size, 16}]).
```

Use least_connections for strict load balancing:
```erlang
shackle_pool:start(my_pool, my_client, ClientOpts,
    [{pool_strategy, least_connections}]).
```

Use random_without_replacement with retries:
```erlang
shackle_pool:start(my_pool, my_client, ClientOpts,
    [{pool_strategy, random_without_replacement}, {max_retries, 5}]).
```

## Performance

| Strategy | Time | Space | Best For |
|----------|------|-------|----------|
| power_of_two | O(1) | O(n) | General workloads (recommended) |
| least_connections | O(n) | O(n) | Critical applications requiring perfect balance |
| random_without_replacement | O(n) | O(n) | Applications with aggressive retry logic |
| consistent_hash | O(1) | O(1) | Stateful backends needing deterministic routing |

See README.md for detailed strategy documentation.
""".

-include("shackle_internal.hrl").
-include("shackle_pool.hrl").

%% Key of the per-request server permutation used by random_without_replacement
-define(PERMUTATION_KEY, '$shackle_permutation').
-define(PROBE_OFFSET_KEY, '$shackle_probe_offset').

-dialyzer({nowarn_function, options/1}).
-dialyzer({nowarn_function, server/3}).
-dialyzer({nowarn_function, server/4}).
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
    server/3,
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
-doc """
Start a connection pool with default options.

Equivalent to `start(Name, Client, ClientOptions, [])`.

See `start/4` for option documentation and strategy selection.
""".
-spec start(shackle_pool:name(), shackle:client(), shackle_client:options()) ->
    ok | {error, shackle_not_started | pool_not_started | pool_already_started}.
start(Name, Client, ClientOptions) ->
    start(Name, Client, ClientOptions, []).

-doc """
Start a connection pool with specified options.

## Options

- `pool_strategy` (default: `random`)
  - `random`: Random connection selection (O(1))
  - `round_robin`: Deterministic rotation (O(1))
  - `power_of_two`: Sample 2 connections, pick less-loaded (O(1), **recommended**)
  - `least_connections`: Pick minimum-loaded (O(n), optimal)
  - `random_without_replacement`: Random permutation, no server repeated on retry (O(n))
  - `consistent_hash`: Hash-based deterministic selection (O(1))

- `pool_size` (default: 16): Number of connections in the pool

- `backlog_size` (default: 1024): Maximum concurrent requests per connection

- `max_retries` (default: 3): Number of retry attempts to find available connection

## Examples

```erlang
% Recommended: power_of_two strategy for general workloads
shackle_pool:start(my_pool, my_client, ClientOpts,
    [{pool_strategy, power_of_two}, {pool_size, 16}]).

% Optimal load balancing (higher CPU)
shackle_pool:start(my_pool, my_client, ClientOpts,
    [{pool_strategy, least_connections}]).

% Smart retries: never try same server twice
shackle_pool:start(my_pool, my_client, ClientOpts,
    [{pool_strategy, random_without_replacement}, {max_retries, 5}]).

% Deterministic routing for stateful backends
shackle_pool:start(my_pool, my_client, ClientOpts,
    [{pool_strategy, consistent_hash}]).
```
""".
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
            validate_strategy(OptionsRec),
            % Normalize servers configuration
            NormalizedClientOpts = normalize_client_options(ClientOptions),
            setup(OptionsRec),
            start_children(Name, Client, NormalizedClientOpts, OptionsRec),
            ok
    end.

%% Load-aware strategies read their load from the backlog semaphores, which do not
%% exist when backlog_size is infinity, so they would always see load 0.
validate_strategy(#pool_options{backlog_size = infinity, pool_strategy = Strategy})
  when Strategy == power_of_two; Strategy == least_connections ->
    error({incompatible_pool_options, {pool_strategy, Strategy}, {backlog_size, infinity}});
validate_strategy(_OptionsRec) ->
    ok.

-spec stop(shackle_pool:name()) ->
    ok | {error, shackle_not_started | pool_not_started}.
stop(Name) ->
    case options(Name) of
        {ok, #pool_options{pool_size = PoolSize} = OptionsRec} ->
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
    Now = erlang:monotonic_time(millisecond),
    wait_until_available2(any, Name, Now, Now + Timeout).

-doc "Wait until the given server pool is available to accept requests".
-spec wait_until_all_available(shackle_pool:name(), non_neg_integer()) -> boolean().
wait_until_all_available(Name, Timeout) when is_integer(Timeout) ->
    Now = erlang:monotonic_time(millisecond),
    wait_until_available2(all, Name, Now, Now + Timeout).

wait_until_available2(_, _Name, Now, Expiration) when Now >= Expiration ->
    false;
wait_until_available2(Method, Name, _Now, Expiration) ->
    case active(Method, Name) of
        false ->
            timer:sleep(10),
            wait_until_available2(Method, Name, erlang:monotonic_time(millisecond), Expiration);
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
            server(1, Options, MaxRetries + 1, #{});
        {error, Reason} ->
            {error, Reason}
    end;
server(#pool_options{max_retries = MaxRetries} = Options) ->
    server(1, Options, MaxRetries + 1, #{}).

-spec server(name() | pool_options(), pos_integer() | map()) ->
    {ok, shackle:client(), atom(), shackle_sema:sema_ref()} |
    {error, atom()}.
server(Name, CountOrOpts) when is_atom(Name) ->
    case options(Name) of
        {ok, #pool_options{max_retries = MaxRetries} = Options} ->
            if
                is_integer(CountOrOpts) -> server(CountOrOpts, Options, MaxRetries + 1, #{});
                is_map(CountOrOpts)     -> server(1, Options, MaxRetries + 1, CountOrOpts)
            end;
        {error, Reason} ->
            {error, Reason}
    end;
server(#pool_options{max_retries = MaxRetries} = Options, CallOptions) when is_map(CallOptions) ->
    %% New API with call options map
    server(1, Options, MaxRetries + 1, CallOptions);
server(#pool_options{max_retries = MaxRetries} = Options, Count) when is_integer(Count) ->
    %% Old API with count
    server(Count, Options, MaxRetries + 1, #{}).

-doc """
Select a server from the pool with optional call options.

The Options map parameter enables strategy-specific and extensible behavior:
- For consistent_hash strategy: include {hash_key => Key} to route consistently
- For other strategies: options are extensible for future enhancements
- Empty map {} uses default behavior

Example with consistent_hash strategy:
```erlang
% Same hash key always maps to same server
{ok, Client, Server, Sema} = shackle_pool:server(PoolOpts, Count, #{hash_key => "user123"})
```
""".
-spec server(name() | pool_options(), pos_integer(), map()) ->
    {ok, shackle:client(), atom(), shackle_sema:sema_ref()} |
    {error, atom()}.
server(Name, Count, Options) when is_atom(Name) ->
    case options(Name) of
        {ok, #pool_options{max_retries = MaxRetries} = Opts} ->
            server(Count, Opts, MaxRetries + 1, Options);
        {error, Reason} ->
            {error, Reason}
    end;
server(#pool_options{max_retries = MaxRetries} = Options, Count, CallOptions) ->
    server(Count, Options, MaxRetries + 1, CallOptions).

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

server(_Count, #pool_options{name = Name, client = Client}, 0, _CallOptions) ->
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
    N,
    CallOptions0
) ->
    CallOptions = init_attempts(PoolStrategy, PoolSize, CallOptions0),
    ServerId = {Name, ServerIdx} = server_id(Name, PoolSize, PoolStrategy, CallOptions),
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
                    server(Count, Options, N - 1, next_attempt(PoolStrategy, PoolSize, CallOptions))
            end;
        false ->
            observe_pool_metric(Client, Name, shackle_attempt_total, <<"disabled">>),
            server(Count, Options, N - 1, next_attempt(PoolStrategy, PoolSize, CallOptions))
    end.

%% random_without_replacement draws one permutation per request and consumes one
%% entry per attempt, so retries walk the pool without repeating a server until
%% every server has been tried.
init_attempts(random_without_replacement, PoolSize, CallOptions)
  when not is_map_key(?PERMUTATION_KEY, CallOptions) ->
    CallOptions#{?PERMUTATION_KEY => shuffle_list(lists:seq(1, PoolSize))};
init_attempts(_PoolStrategy, _PoolSize, CallOptions) ->
    CallOptions.

next_attempt(random_without_replacement, _PoolSize,
             #{?PERMUTATION_KEY := [_ | [_ | _] = Rest]} = CallOptions) ->
    CallOptions#{?PERMUTATION_KEY := Rest};
next_attempt(random_without_replacement, PoolSize, CallOptions) ->
    %% Every server has been tried; start a new cycle
    CallOptions#{?PERMUTATION_KEY := shuffle_list(lists:seq(1, PoolSize))};
%% Retries under consistent_hash probe the following indexes, so a key whose
%% primary connection is down does not burn every attempt on that one connection.
next_attempt(consistent_hash, _PoolSize, CallOptions) ->
    CallOptions#{?PROBE_OFFSET_KEY => maps:get(?PROBE_OFFSET_KEY, CallOptions, 0) + 1};
next_attempt(_PoolStrategy, _PoolSize, CallOptions) ->
    CallOptions.

-doc "Emit observability event for pool metrics".
observe_pool_metric(Client, Pool, Metric, Reason) ->
    shackle_observe:event([metric, counter], #{count => 1}, #{
        metric => Metric,
        client => Client,
        pool   => Pool,
        reason => Reason
    }).

-doc """
Get the current load (backlog count) for a server.

Returns the number of active requests in the server's backlog.
Used by power_of_two and least_connections strategies for load-aware selection.

Falls back to 0 if semaphore doesn't exist (e.g., infinite backlog mode).
""".
-spec get_server_load(shackle_pool:name(), pos_integer()) -> non_neg_integer().
get_server_load(Name, ServerIdx) ->
    try shackle_sema:count(Name, ServerIdx) of
        Count -> Count
    catch
        _:_ -> 0  %% Fallback to 0 if semaphore doesn't exist (e.g., infinite backlog)
    end.

-doc """
Shuffle a list into random order using Fisher-Yates algorithm.

Used by random_without_replacement strategy to generate permutations
that ensure retries never try the same server twice before cycling.

Compatible with all OTP versions - uses built-in rand:shuffle on OTP 29+,
manual Fisher-Yates implementation on older versions.

## Example

```erlang
shuffle_list([1, 2, 3, 4]) -> [3, 1, 4, 2]
shuffle_list([1, 2, 3, 4]) -> [2, 4, 1, 3]  % Different order each call
```
""".
%% Shuffle using OTP 29+ built-in or manual fallback
-if(defined(OTP_RELEASE) andalso ?OTP_RELEASE >= 29).
-spec shuffle_list(list()) -> list().
shuffle_list(List) ->
    rand:shuffle(List).
-else.
-spec shuffle_list(list()) -> list().
shuffle_list(List) ->
    shuffle_list_manual(List, []).

shuffle_list_manual([], Acc) ->
    Acc;
shuffle_list_manual(List, Acc) ->
    Idx = shackle_utils:random(length(List)),
    {Selected, Rest} = select_nth(List, Idx),
    shuffle_list_manual(Rest, [Selected | Acc]).

-doc """
Select the Nth element from a list and return {Element, RestOfList}.

Helper for Fisher-Yates shuffle implementation. Extracts element at position N
and returns it paired with the remaining list.

## Example

```erlang
select_nth([a, b, c, d], 2) -> {b, [a, c, d]}
select_nth([a, b, c, d], 1) -> {a, [b, c, d]}
```
""".
-spec select_nth(list(), pos_integer()) -> {term(), list()}.
select_nth(List, N) ->
    select_nth(List, N, []).

select_nth([H | T], 1, Acc) ->
    {H, lists:reverse(Acc, T)};
select_nth([H | T], N, Acc) ->
    select_nth(T, N - 1, [H | Acc]).
-endif.

-doc """
Select a server index from the pool using the configured strategy.

## Strategies

**random** - Pure random selection
- Example: Server loads [2, 1, 5, 3] → Any server equally likely

**round_robin** - Deterministic rotation
- Example: Selects 1, 2, 3, 4, 1, 2, 3, 4, ... in order

**power_of_two** - Near-optimal load balancing (recommended)
- Compares 2 random servers, selects the less-loaded
- Example: Compare servers 1 and 4 with loads [2, 1, 5, 3] → Select server 4 (load=3)
- Balances between speed (O(1)) and quality (near-optimal distribution)

**least_connections** - Optimal load balancing
- Scans all active servers, selects minimum-loaded
- Example: Servers with loads [2, 1, 5, 3] → Select server 2 (load=1)
- Best balance but O(n) cost

**random_without_replacement** - Smart retries
- Generates random permutation [3, 1, 4, 2]
- Retries never try same server twice before cycling
- Example: Retry sequence tries 3, then 1, then 4, then 2 (never repeats until full cycle)
- Guarantees coverage of all servers before repetition

**consistent_hash** - Deterministic routing
- Hash-based selection produces same server for each time window
- Example: Time-based hash always routes to same server during same second
- Enables session affinity for stateful backends
""".
server_id(Name, PoolSize, random, _CallOptions) ->
    {Name, shackle_utils:random(PoolSize)};

server_id(Name, PoolSize, round_robin, _CallOptions) ->
    UpdateOps = [{2, 1, PoolSize, 1}],
    Key = {Name, round_robin},
    [ServerId] = ets:update_counter(?ETS_TABLE_POOL_INDEX, Key, UpdateOps),
    {Name, ServerId};

server_id(Name, PoolSize, power_of_two, _CallOptions) ->
    %% Select 2 random servers and return the one with fewer active requests
    %% Near-optimal load balancing with O(1) cost
    Idx1 = shackle_utils:random(PoolSize),
    Idx2 = shackle_utils:random(PoolSize),
    Load1 = load_for_selection(Name, Idx1),
    Load2 = load_for_selection(Name, Idx2),
    SelectedIdx = if Load1 =< Load2 -> Idx1; true -> Idx2 end,
    {Name, SelectedIdx};

server_id(Name, PoolSize, least_connections, _CallOptions) ->
    %% Find the server with the least active requests among active servers
    %% Optimal load balancing - scans all servers once (O(n))
    %% Optimized: Find min_by_load in same pass as checking active servers
    case find_min_load_active_server(Name, PoolSize) of
        undefined ->
            %% No active servers, fallback to random
            {Name, shackle_utils:random(PoolSize)};
        MinServer ->
            {Name, MinServer}
    end;

server_id(Name, _PoolSize, random_without_replacement, CallOptions) ->
    %% Head of the request's permutation (see init_attempts/3 and next_attempt/3)
    [SelectedIdx | _] = maps:get(?PERMUTATION_KEY, CallOptions),
    {Name, SelectedIdx};

server_id(Name, PoolSize, consistent_hash, CallOptions) ->
    %% Hash-based deterministic selection
    %% Uses caller-provided key for session affinity when available
    %% Falls back to random if no key provided (for backward compatibility)
    %% Enables session affinity for stateful backends
    case maps:get(hash_key, CallOptions, undefined) of
        undefined ->
            %% Fallback to random when no hash key provided
            {Name, shackle_utils:random(PoolSize)};
        HashKey ->
            %% Use provided key for deterministic routing. Retries move to the
            %% following indexes (see next_attempt/3).
            Offset = maps:get(?PROBE_OFFSET_KEY, CallOptions, 0),
            {Name, (erlang:phash2(HashKey, PoolSize) + Offset) rem PoolSize + 1}
    end.

%% Load used to rank a sampled server in power_of_two. An inactive server ranks
%% last: its semaphore count is 0 while it is down, so a raw load would make it
%% look idle. It is only chosen when both samples are inactive, and server/4 then
%% moves on to the next attempt.
load_for_selection(Name, Idx) ->
    case shackle_status:active({Name, Idx}) of
        true -> get_server_load(Name, Idx);
        false -> infinity
    end.

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

%% A child that fails to start (for example, a bad server entry) leaves the pool
%% unusable, so tear down what was set up and fail the pool start.
start_children(Name, Client, ClientOptions, #pool_options {pool_size = PoolSize} = OptionsRec) ->
    Results = [supervisor:start_child(?SUPERVISOR,
        server_spec(Name, Index, Client, ClientOptions)) ||
        Index <- lists:seq(1, PoolSize)],
    case [Reason || {error, Reason} <- Results] of
        [] ->
            ok;
        [FirstReason | _] ->
            stop_children(Name, lists:seq(1, PoolSize)),
            cleanup(Name, OptionsRec),
            error({pool_start_failed, Name, FirstReason})
    end.

stop_children(_Name, []) ->
    ok;
stop_children(Name, [Index | T]) ->
    ServerName = server_name(Name, Index),
    supervisor:terminate_child(?SUPERVISOR, ServerName),
    supervisor:delete_child(?SUPERVISOR, ServerName),
    stop_children(Name, T).

%%--------------------------------------------------------------------
%% Find the active server with minimum load in a single pass.
%%
%% Scans all servers starting at a random index and wrapping around, tracking the
%% server with minimum load. Ties go to the first server in scan order, so equal
%% loads (for example, an idle pool) are spread across servers instead of always
%% favouring one end of the pool.
%% Returns the server index with minimum load, or undefined if no active servers.
%%
%% Example
%%   Servers = [1, 2, 3, 4]
%%   Loads =   [2, 1, 5, 3]
%%   Result: 2  (Server 2 has minimum load of 1)
find_min_load_active_server(Name, PoolSize) ->
    Start = shackle_utils:random(PoolSize),
    Order = lists:seq(Start, PoolSize) ++ lists:seq(1, Start - 1),
    find_min_load_active_server(Name, Order, undefined, infinity).

find_min_load_active_server(_Name, [], MinServer, _MinLoad) ->
    MinServer;
find_min_load_active_server(Name, [I | Rest], MinServer, MinLoad) ->
    case shackle_status:active({Name, I}) of
        true ->
            Load = get_server_load(Name, I),
            if
                Load < MinLoad -> find_min_load_active_server(Name, Rest, I, Load);
                true           -> find_min_load_active_server(Name, Rest, MinServer, MinLoad)
            end;
        false ->
            find_min_load_active_server(Name, Rest, MinServer, MinLoad)
    end.

%%--------------------------------------------------------------------
%% Normalize client options to support both single server and multi-server configs.
%%
%% If 'servers' list is provided, it normalizes it using shackle_servers:new/2.
%% If only 'address' and 'port' are provided, converts them to 'servers' list format.
normalize_client_options(ClientOptions) ->
    %% Merge app-env client defaults first, so a port or servers list set only in
    %% the app env is seen here. shackle_server:init merges them again, which is a no-op.
    Options = shackle_utils:default_options(client, ClientOptions),
    case ?LOOKUP(servers, Options, undefined) of
        undefined ->
            % No servers list - return as-is for backwards compatibility
            % The validation will happen at shackle_server:init time
            ClientOptions;
        ServersList ->
            % Servers list provided, normalize it. A default port is only needed
            % for bare hostnames; entries of the form {Host, Port} do not need one.
            Port = ?LOOKUP(port, Options, undefined),
            try shackle_servers:new(ServersList, Port) of
                ServersState ->
                    NormalizedServers = tuple_to_list(maps:get(servers, ServersState)),
                    [{servers, NormalizedServers} |
                     lists:keydelete(servers, 1, Options)]
            catch
                error:Error ->
                    error({invalid_servers_config, Error, ServersList})
            end
    end.
