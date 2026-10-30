-module(shackle_sup).
-include("shackle_internal.hrl").

%% internal
-export([
    start_link/0
]).

-behaviour(supervisor).
-export([
    init/1
]).

%% internal
-spec start_link() ->
    {ok, pid()}.

start_link() ->
    supervisor:start_link({local, ?SUPERVISOR}, ?SUPERVISOR, []).

%% supervisor callbacks
-spec init([]) ->
    {ok, {{one_for_one, 5, 10}, [supervisor:child_spec()]}}.

init([]) ->
    shackle_hooks:init(),
    shackle_pool:init(),
    shackle_status:init(),

    % Resolve observability backend from app environment
    {ObsModule, ObsOpts} = case application:get_env(shackle, observability, nil) of
        undefined                     -> {nil, nil};
        nil                           -> {nil, nil};
        telemetry                     -> {shackle_observe_telemetry, nil};
        prometheus                    -> {shackle_observe_prometheus, nil};
        {telemetry, Opts}             -> {shackle_observe_telemetry, Opts};
        {prometheus, Opts}            -> {shackle_observe_prometheus, Opts};
        Mod when is_atom(Mod)         -> {Mod, nil};
        {Mod, Opts} when is_atom(Mod) -> {Mod, Opts}
    end,

    % Start observability if configured
    ChildSpecs = case ObsModule of
        nil ->
            [?CHILD(shackle_ets_manager)];
        _ ->
            [
                #{
                    id       => shackle_observe,
                    start    => {shackle_observe, start_link, [ObsModule, ObsOpts]},
                    restart  => permanent,
                    shutdown => 1000,
                    type     => worker,
                    modules  => [shackle_observe]
                },
                ?CHILD(shackle_ets_manager)
            ]
    end,

    {ok, {{one_for_one, 5, 10}, ChildSpecs}}.
