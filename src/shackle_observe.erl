-module(shackle_observe).
-include_lib("kernel/include/logger.hrl").

-behaviour(gen_server).

%% API
-export([
    start_link/2,
    span/3,
    event/2,
    event/3,
    enabled/0,
    dispatcher/0
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2
]).

%% Callback definitions for backend modules
-callback start(term()) -> ok.
-callback stop() -> ok.
-callback event([atom()], map(), map()) -> ok.

-define(SERVER, ?MODULE).

-type state() :: module() | nil.

-export_type([state/0]).

%%% Public API

-doc """
Start the observability singleton process. `Module` is the backend implementation
(or `undefined`/`nil` to disable observability). `Options` is passed to Module:start/1.
Typically called by shackle_sup during app startup.
""".
-spec start_link(module() | undefined | nil, term()) -> {ok, pid()} | {error, term()}.
start_link(Module, Options) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, {Module, Options}, []).

-doc """
Runs `Fun/0`, emitting `[shackle | EventNameSuffix] ++ [start]` before and
`[shackle | EventNameSuffix] ++ [stop]` after (or `++ [exception]` if `Fun` raises).
If no backend is configured, just runs `Fun/0` with no overhead.
""".
-spec span([atom()], map(), fun(() -> term())) -> term().
span(EventNameSuffix, StartMetadata, Fun) when is_list(EventNameSuffix), is_function(Fun, 0) ->
    case backend() of
        nil ->
            Fun();
        Backend ->
            StartTime = erlang:monotonic_time(),
            Backend:event([shackle | EventNameSuffix] ++ [start],
                         #{monotonic_time => StartTime}, StartMetadata),
            try Fun() of
                Result ->
                    StopTime = erlang:monotonic_time(),
                    Backend:event([shackle | EventNameSuffix] ++ [stop],
                                 #{duration => StopTime - StartTime, monotonic_time => StopTime},
                                 StartMetadata),
                    Result
            catch Class:Reason:Stacktrace ->
                StopTime = erlang:monotonic_time(),
                Backend:event([shackle | EventNameSuffix] ++ [exception],
                             #{duration => StopTime - StartTime, monotonic_time => StopTime},
                             StartMetadata#{kind => Class, reason => Reason}),
                erlang:raise(Class, Reason, Stacktrace)
            end
    end.

-doc "Whether an observability backend is currently configured.".
-spec enabled() -> boolean().
enabled() ->
    backend() =/= nil.

-doc """
Returns the dispatcher module for the current observability configuration.
`shackle_observe_noop` when disabled; `shackle_observe_span` when enabled.
""".
-spec dispatcher() -> shackle_observe_noop | shackle_observe_span.
dispatcher() ->
    case backend() of
        nil -> shackle_observe_noop;
        _   -> shackle_observe_span
    end.

-doc "Equivalent to `event/3` with `Measurements = #{}`".
-spec event([atom()], map()) -> ok.
event(EventNameSuffix, Metadata) ->
    event(EventNameSuffix, #{}, Metadata).

-doc """
Emits `[shackle | EventNameSuffix]` with the given measurements/metadata
through the configured backend. A no-op if no backend is configured.
""".
-spec event([atom()], map(), map()) -> ok.
event(EventNameSuffix, Measurements, Metadata) when is_list(EventNameSuffix) ->
    case backend() of
        nil     -> ok;
        Backend -> Backend:event([shackle | EventNameSuffix], Measurements, Metadata)
    end.

%%% gen_server callbacks

-spec init({module() | undefined | nil, map()}) -> {ok, state()}.
init({Module, Options}) ->
    process_flag(trap_exit, true),
    ResolvedModule = resolve_module(Module),
    case ResolvedModule of
        nil ->
            persistent_term:put(?SERVER, nil),
            {ok, _Backend = nil};
        _ ->
            case code:ensure_loaded(ResolvedModule) of
                {module, ResolvedModule} ->
                    try
                        ok = ResolvedModule:start(Options),
                        persistent_term:put(?SERVER, ResolvedModule),
                        {ok, ResolvedModule}
                    catch E:R:ST ->
                        ?LOG_ERROR("Failed to start observability backend ~p: ~p:~p",
                                  [ResolvedModule, E, R]),
                        erlang:raise(E, R, ST)
                    end;
                {error, _Reason} ->
                    ?LOG_WARNING("Observability backend module ~p not found, disabling observability",
                                [ResolvedModule]),
                    persistent_term:put(?SERVER, nil),
                    {ok, _Backend = nil}
            end
    end.

-spec handle_call(term(), {pid(), reference()}, state()) -> {reply, {error, not_supported}, state()}.
handle_call(_Request, _From, State) ->
    {reply, {error, not_supported}, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(_Request, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {stop, term(), state()} | {noreply, state()}.
handle_info({'EXIT', _Pid, Reason}, Backend) when Reason == normal; Reason == shutdown ->
    stop_backend(Backend),
    {stop, Reason, Backend};
handle_info({'EXIT', _Pid, Reason}, Backend) ->
    stop_backend(Backend),
    {stop, {error, Reason}, Backend};
handle_info(_Info, Backend) ->
    {noreply, Backend}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, Backend) ->
    stop_backend(Backend),
    ok.

%%% Internal functions

-spec resolve_module(module() | undefined | nil) -> module() | nil.
resolve_module(undefined) -> nil;
resolve_module(nil)       -> nil;
resolve_module(telemetry) -> shackle_observe_telemetry;
resolve_module(prometheus) -> shackle_observe_prometheus;
resolve_module(Mod) when is_atom(Mod) -> Mod;
resolve_module(_) -> nil.

-spec stop_backend(module() | nil) -> ok.
stop_backend(nil) -> ok;
stop_backend(Backend) ->
    try
        case erlang:function_exported(Backend, stop, 0) of
            true  -> Backend:stop();
            false -> ok
        end
    after
        persistent_term:erase(?SERVER)
    end.

-spec backend() -> module() | nil.
backend() ->
    case persistent_term:get(?SERVER, false) of
        false ->
            % Not yet initialized; look at app env and cache
            Module = case application:get_env(shackle, observability, nil) of
                undefined       -> nil;
                nil             -> nil;
                telemetry       -> shackle_observe_telemetry;
                prometheus      -> shackle_observe_prometheus;
                M when is_atom(M) -> M
            end,
            persistent_term:put(?SERVER, Module),
            Module;
        M ->
            M
    end.
