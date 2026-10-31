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
    server/1,
    terminate/0
]).

%% types
-type max_retries() :: non_neg_integer().
-type name() :: atom().
-type pool_size() :: pos_integer().
-type pool_strategy() :: power_of_two | random | round_robin.
-type option() :: {backlog_size, shackle_backlog:backlog_size()} |
                  {max_retries, max_retries()} |
                  {pool_size, pool_size()} |
                  {pool_strategy, pool_strategy()}.
-type options() :: [option()].

-export_type([
    max_retries/0,
    name/0,
    options/0,
    pool_size/0,
    pool_strategy/0
]).

%% public
-spec start(shackle_pool:name(), shackle:client(), shackle_client:options()) ->
    ok | {error, shackle_not_started | pool_already_started |
        invalid_backlog_size | term()}.

start(Name, Client, ClientOptions) ->
    start(Name, Client, ClientOptions, []).

-spec start(shackle_pool:name(), shackle:client(), shackle_client:options(), options()) ->
    ok | {error, shackle_not_started | pool_already_started |
        invalid_backlog_size | term()}.

%% power_of_two compares the backlog counters, and the counters are
%% not kept when backlog_size is infinity.
start(Name, Client, ClientOptions, Options) ->
    case {?LOOKUP(pool_strategy, Options, ?DEFAULT_POOL_STRATEGY),
          ?LOOKUP(backlog_size, Options, ?DEFAULT_BACKLOG_SIZE)} of
        {power_of_two, infinity} ->
            {error, invalid_backlog_size};
        _ ->
            start_pool(Name, Client, ClientOptions, Options)
    end.

-spec stop(shackle_pool:name()) ->
    ok | {error, shackle_not_started | pool_not_started}.

stop(Name) ->
    case options(Name) of
        {ok, #pool_options {server_names = ServerNames}} ->
            stop_children(tuple_to_list(ServerNames)),
            persistent_term:erase(key(Name)),
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

%% internal
-spec init() ->
    ok.

init() ->
    erase_all().

-spec server(shackle_pool:name()) ->
    {ok, shackle:client(), atom()} |
    {error, pool_not_started | no_server | shackle_not_started}.

server(Name) ->
    case options(Name) of
        {ok, #pool_options {max_retries = MaxRetries} = Options} ->
            server(Options, MaxRetries + 1);
        {error, Reson} ->
            {error, Reson}
    end.

-spec terminate() ->
    ok.

terminate() ->
    erase_all().

%% private
erase_all() ->
    [persistent_term:erase(Key) ||
        {{?MODULE, _Name} = Key, _} <- persistent_term:get()],
    ok.

key(Name) ->
    {?MODULE, Name}.

options(Name) ->
    case persistent_term:get(key(Name), undefined) of
        undefined ->
            case whereis(?SUPERVISOR) of
                undefined ->
                    {error, shackle_not_started};
                _Pid ->
                    {error, pool_not_started}
            end;
        Options ->
            {ok, Options}
    end.

options_rec(Name, Client, Options) ->
    BacklogSize = ?LOOKUP(backlog_size, Options, ?DEFAULT_BACKLOG_SIZE),
    MaxRetries = ?LOOKUP(max_retries, Options, ?DEFAULT_MAX_RETRIES),
    PoolSize = ?LOOKUP(pool_size, Options, ?DEFAULT_POOL_SIZE),
    PoolStrategy = ?LOOKUP(pool_strategy, Options, ?DEFAULT_POOL_STRATEGY),

    #pool_options {
        backlog = shackle_backlog:new(PoolSize),
        backlog_size = BacklogSize,
        client = Client,
        max_retries = MaxRetries,
        name = Name,
        pool_size = PoolSize,
        pool_strategy = PoolStrategy,
        round_robin = round_robin(PoolStrategy),
        server_names = list_to_tuple([server_name(Name, N) ||
            N <- lists:seq(1, PoolSize)]),
        status = shackle_status:new(PoolSize)
    }.

%% Use one counter for each scheduler, so callers do not write to the
%% same slot. Each counter starts at a different value, so the first
%% picks go to different servers.
round_robin(round_robin) ->
    Schedulers = erlang:system_info(schedulers),
    Ref = atomics:new(Schedulers * ?STRIDE, [{signed, false}]),
    [atomics:put(Ref, (N - 1) * ?STRIDE + 1, N - 1) ||
        N <- lists:seq(1, Schedulers)],
    Ref;
round_robin(power_of_two) ->
    undefined;
round_robin(random) ->
    undefined.

server(#pool_options {
        client = Client
    }, 0) ->

    shackle_telemetry:no_server(Client),
    {error, no_server};
server(#pool_options {
        backlog = Backlog,
        backlog_size = BacklogSize,
        client = Client,
        server_names = ServerNames,
        status = Status
    } = Options, N) ->

    Index = server_index(Options),
    case shackle_status:active(Status, Index) of
        true ->
            case shackle_backlog:check(Backlog, Index, BacklogSize) of
                true ->
                    {ok, Client, element(Index, ServerNames)};
                false ->
                    shackle_telemetry:backlog_full(Client),
                    server(Options, N - 1)
            end;
        false ->
            shackle_telemetry:disabled(Client),
            server(Options, N - 1)
    end.

server_index(#pool_options {
        pool_size = PoolSize,
        pool_strategy = random
    }) ->

    shackle_utils:random(PoolSize);
server_index(#pool_options {
        pool_size = 1,
        pool_strategy = power_of_two
    }) ->

    1;
server_index(#pool_options {
        backlog = Backlog,
        pool_size = PoolSize,
        pool_strategy = power_of_two,
        status = Status
    }) ->

    %% Pick two different servers. If both picks are the same slow
    %% server, that server gets the request.
    A = shackle_utils:random(PoolSize),
    B = case shackle_utils:random(PoolSize - 1) of
        N when N >= A -> N + 1;
        N -> N
    end,
    case server_load(Backlog, Status, A) =< server_load(Backlog, Status, B) of
        true -> A;
        false -> B
    end;
server_index(#pool_options {
        pool_size = PoolSize,
        round_robin = RoundRobin
    }) ->

    Ix = (erlang:system_info(scheduler_id) - 1) * ?STRIDE + 1,
    atomics:add_get(RoundRobin, Ix, 1) rem PoolSize + 1.

%% A disabled server has an empty backlog. Without this, power_of_two
%% would prefer it and the request would fail with no_server.
server_load(Backlog, Status, Index) ->
    case shackle_status:active(Status, Index) of
        true -> shackle_backlog:load(Backlog, Index);
        false -> infinity
    end.

server_name(Name, Index) ->
    list_to_atom(atom_to_list(Name) ++ "_" ++ integer_to_list(Index)).

server_spec(Index, ClientOptions, #pool_options {
        server_names = ServerNames
    } = OptionsRec) ->

    ServerName = element(Index, ServerNames),
    ServerOpts = {Index, ClientOptions, OptionsRec},
    StartFunc = {?SERVER, start_link, [ServerName, ServerOpts]},
    {ServerName, StartFunc, permanent, 5000, worker, [?SERVER]}.

start_children(ClientOptions, #pool_options {
        pool_size = PoolSize
    } = OptionsRec) ->

    start_children(1, PoolSize, ClientOptions, OptionsRec).

start_children(Index, PoolSize, _ClientOptions, _OptionsRec)
        when Index > PoolSize ->

    ok;
start_children(Index, PoolSize, ClientOptions, OptionsRec) ->
    case supervisor:start_child(?SUPERVISOR,
            server_spec(Index, ClientOptions, OptionsRec)) of
        {ok, _Pid} ->
            start_children(Index + 1, PoolSize, ClientOptions, OptionsRec);
        {error, Reason} ->
            {error, Reason}
    end.

start_pool(Name, Client, ClientOptions, Options) ->
    case options(Name) of
        {ok, _OptionsRec} ->
            {error, pool_already_started};
        {error, shackle_not_started} ->
            {error, shackle_not_started};
        {error, pool_not_started} ->
            OptionsRec = options_rec(Name, Client, Options),
            persistent_term:put(key(Name), OptionsRec),
            case start_children(ClientOptions, OptionsRec) of
                ok ->
                    ok;
                {error, Reason} ->
                    stop(Name),
                    {error, Reason}
            end
    end.

stop_children([]) ->
    ok;
stop_children([ServerName | T]) ->
    supervisor:terminate_child(?SUPERVISOR, ServerName),
    supervisor:delete_child(?SUPERVISOR, ServerName),
    stop_children(T).
