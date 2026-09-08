-module(shackle_utils).
-include("shackle_internal.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% public
-export([
    ets_options/0,
    info_msg/3,
    lookup/3,
    random/1,
    random_element/1,
    warning_msg/3,
    default_options/2,
    merge_options/1,
    merge_options/2,
    merge_options/3,
    on_bounce_event/3,
    list_peers/0,
    list_peers/1,
    peername/1,
    sock_owner/1
]).

%% NOTE: use ?WARN(PoolName, Format, Data) macro instead
-deprecated([warning_msg/3]).

%% public
-spec ets_options() ->
  [atom() | {atom(), any()}].

-ifdef(DECENTRALIZED_COUNTERS).

ets_options() -> [
      named_table,
      public,
      {write_concurrency, true},
      {decentralized_counters, true}
  ].

-else.

ets_options() -> [
      named_table,
      public,
      {write_concurrency, true}
  ].

-endif.

-spec info_msg(shackle_pool:name(), string(), [term()]) ->
    ok.

info_msg(Pool, Format, Data) ->
    error_logger:info_msg("[~p] " ++ Format, [Pool | Data]).

-spec lookup(atom(), [{atom(), term()}], term()) ->
    term().

lookup(Key, List, Default) ->
    case lists:keyfind(Key, 1, List) of
        false -> Default;
        {_, Value} -> Value
    end.

-spec random(pos_integer()) ->
    non_neg_integer().

random(1) -> 1;
random(N) ->
    rand:uniform(N).

-spec random_element([term()]) ->
    term().

random_element([X]) ->
    X;
random_element(L) when is_list(L) ->
    I = length(L),
    lists:nth(rand:uniform(I), L).

-spec warning_msg(shackle_pool:name(), string(), [term()]) ->
    ok.

warning_msg(Pool, Format, Data) ->
    ?WARN(Pool, Format, Data).

-doc """
List sockets grouped by peer address/port, in the descending order of count This
function is mainly used for debugging.
""".
-spec list_peers() -> [{integer(), binary()}].
list_peers() ->
    Endpoints = lists:filtermap(fun(S) ->
        case peername(S) of
            PN when is_binary(PN) ->
                {true, PN};
            _ ->
                false
        end
    end, sockets()),
    M = lists:foldl(fun(PN, M) -> maps:update_with(PN, fun(V) -> V+1 end, 1, M) end, #{}, Endpoints),
    L = lists:sort(fun({_, V1}, {_, V2}) -> V2 =< V1 end, maps:to_list(M)),
    [{V, PN} || {PN, V} <- L].

-spec list_peers(non_neg_integer()) -> list().
list_peers(Limit) when is_integer(Limit) ->
    lists:sublist(list_peers(), Limit).

-spec peername(port() | tuple()) -> binary() | atom().
peername(S) when is_port(S) ->
    case inet:peername(S) of
        {ok, {A, P}} ->
            list_to_binary(lists:flatten(io_lib:format("~s:~w", [inet_parse:ntoa(A), P])));
        {error, Reason} ->
            Reason
    end;
peername({_, _, {_Pid, {'$socket', _} = S}}) ->
    peername(S);
peername(S) ->
    case socket:peername(S) of
        {ok, #{addr := A, port := P}} ->
            list_to_binary(lists:flatten(io_lib:format("~s:~w", [inet_parse:ntoa(A), P])));
        {error, Reason} ->
            Reason
    end.

sockets() ->
    socket:which_sockets() ++
    port_list("tcp_inet") ++
    port_list("udp_inet").

sock_info(S) when is_port(S) -> inet:info(S);
sock_info({_, _, {_Pid, {'$socket', _} = S}}) -> sock_info(S);
sock_info(S) -> socket:info(S).

-spec sock_owner(port() | tuple()) -> pid().
sock_owner(S) ->
    maps:get(owner, sock_info(S), not_found).

port_list(Name) ->
    lists:filter(
        fun(Port) ->
            case erlang:port_info(Port, name) of
            {name, Name} -> true;
            _ -> false
            end
        end, erlang:ports()).

-doc """
Function used for troubleshooting shackle bounce event feature. It can be set in
configuration `{shackle, [{on_bounce_event, {shackle_utils,
on_bounce_event}}]}`.
""".
-spec on_bounce_event(atom(), integer(), map()) -> ok.
on_bounce_event(Pool, SrvIdx, Event) ->
    ?LOG_INFO("[~p:~w] shackle bounce event: ~p", [Pool, SrvIdx, Event]).

-doc """
Return global shackle options that get overriden by `Options` that are provided
by a shackle client library upon startup of a client or pool.
""".
-spec default_options(client|pool, map()|[{atom(), any()}]) -> [{atom(), any()}].
default_options(Node, Options) when Node==pool; Node==client ->
    DefOptions = ?GET_ENV(Node, []),
    maps:to_list(maps:merge(to_map(DefOptions), to_map(Options))).

-spec merge_options([{atom(), any()}] | map()) ->
    {[{atom(), any()}], [{atom(), any()}]}.
merge_options(AppShackleOptions) ->
    merge_options(application:get_application(), to_map(AppShackleOptions), #{}).

-doc """
Shackle clients can use this function to merge their provided options for a
shackle client and pool with the global shackle options defined in the `shackle`
application.  The global options are overriden by the client options passed to
this function. If a client provides an option `{Option, Value}` in the `Options`
list with the value equal to the default value for that option in shackle, and
shackle defines a global value different from this default, then the global
value will be used.

The function will throw exception if the any of the `shackle` option names are
invalid or any option value has a wrong type.

## Example:

Given the call `shackle_utils:merge_options(barker, [{pool_size,
3}])`:

1. In this config `pool_size = 2` will be used because 16 is the default for the
pool size:
```
[{shackle, [{pool, [{pool_size, 2}]}]}].
```

2. In this config `pool_size = 4` will be used, because it's defined in the
`barker` application's top-level config:
```
[
  {shackle, [{pool, [{pool_size, 2}]}]},
  {barker, [{pool_size, 4}]}
].
```

3. In this config `pool_size = 6` will be used, because it's defined in the
`barker` application's top-level config, which overrides the `{barker,
[{shackle, ...}]}` config:
```
[
  {shackle, [{pool, [{pool_size, 2}]}]},
  {barker, [{shackle, [{pool, [{pool_size, 4}]}}]}]},
  {barker, [{pool_size, 6}]}
].
```

4. In this config `pool_size = 4` will be used, because it's defined in the
`barker` application's `shackle` config, which overrides the global `shackle`
config:
```
[
  {shackle, [{pool, [{pool_size, 2}]}]},
  {barker, [{shackle, [{pool, [{pool_size, 4}]}}]}]}
].
```

5. In this config `pool_size = 2` will be used, because it's defined in the
the global `shackle` config:
```
[ {shackle, [{pool, [{pool_size, 2}]}]} ].
```

6. In this case the `pool_size = 3` value is derived from the default
options passed to the `merge_options/2`: `[].` (derived as default when
merge_options is called with DefaultOpts=[pool_size=3])
""".
-spec merge_options(atom(), [{atom(), any()}] | map()) ->
    {[{atom(), any()}], [{atom(), any()}]}.
merge_options(App, DefaultOpts) when is_atom(App) ->
    merge_options(App, to_map(DefaultOpts), #{}).

-doc """
Same as `merge_options/2`, but also allows to map option key names.

The `KeyMap` argument allows the client to define key mapping between the
shackle configuration options and the configuration key names in client
environment.

## Example:

```
Config:
[
  {shackle, [{pool, [{pool_size, 2}]}]},
  {barker, [{barker_pool_size, 4}]}
].

shackle_utils:merge_options(barker, [{pool_size, 3}],
    #{pool_size => barker_pool_size})     % Returns 4
```
""".
-spec merge_options(atom() | [{atom(), any()}] | map(), [{atom(), any()}] | map(), #{atom() => atom()}) ->
    {[{atom(), any()}], [{atom(), any()}]}.
merge_options(App, DefaultOpts, KeyMap) when is_atom(App) ->
    AppEnv = application:get_all_env(App),
    merge_options(AppEnv, DefaultOpts, KeyMap);

merge_options(AppEnv, Defaults, KeyMap) when
    (is_list(AppEnv) orelse is_map(AppEnv)),
    (is_list(Defaults) orelse is_map(Defaults)),
    is_map(KeyMap)
->
    validate_options(Defaults),

    % (1) Application's top-level options override application options
    % under the `shackle' environment entry.
    % (2) Application's options under `shackle' override shackle global options
    % defined in the `shackle' application
    % (3) Default options
    Env = to_map(AppEnv),
    AppEnvOpts0 = maps:get(shackle, Env, []),
    AppEnvOpts = #{
        pool =>
            maps:merge(
                to_map(application:get_env(shackle, pool, [])),
                to_map(proplists:get_value(pool, AppEnvOpts0, []))),
        client =>
            maps:merge(
                to_map(application:get_env(shackle, client, [])),
                to_map(proplists:get_value(client, AppEnvOpts0, [])))
    },

    DefaultOpts = to_map(Defaults),

    F = fun({I, {Type, D}}, A) ->
        % Translate the key if a key map is provided
        K = maps:get(I, KeyMap, I),
        case maps:get(K, Env, undefined) of
            V when V == undefined ->
                Opts = maps:get(Type, AppEnvOpts),
                Def = maps:get(I, DefaultOpts, D),
                case maps:get(I, Opts, Def) of
                    DV when DV == undefined; DV =:= D ->
                        A;
                    DV ->
                        [{I, DV} | A]
                end;
            V ->
                [{I, V} | A]
        end
    end,
    PoolConfig = lists:foldl(F, [], [
        {backlog_size, {pool, ?DEFAULT_BACKLOG_SIZE}},
        {max_retries, {pool, ?DEFAULT_MAX_RETRIES}},
        {pool_size, {pool, ?DEFAULT_POOL_SIZE}},
        {pool_strategy, {pool, ?DEFAULT_POOL_STRATEGY}}
    ]),
    ClientConfig = lists:foldl(F, [], [
        {ip, {client, ?DEFAULT_ADDRESS}},
        {port, {client, 0}},
        {protocol, {client, ?DEFAULT_PROTOCOL}},
        {service_name, {client, undefined}},
        {reconnect, {client, ?DEFAULT_RECONNECT}},
        {reconnect_time_max, {client, ?DEFAULT_RECONNECT_MAX}},
        {reconnect_time_min, {client, ?DEFAULT_RECONNECT_MIN}},
        {socket_options, {client, ?DEFAULT_SOCKET_OPTS}},
        {bounce_interval_secs, {client, ?DEFAULT_BOUNCE_INTERVAL}},
        {bounce_udp, {client, ?DEFAULT_BOUNCE_UDP}},
        {max_requests, {client, ?DEFAULT_MAX_REQUESTS}},
        {on_bounce_event, {client, undefined}}
    ]),
    validate_options(PoolConfig),
    validate_options(ClientConfig),
    {ClientConfig, PoolConfig}.

to_map(undefined)         -> #{};
to_map(L) when is_list(L) -> maps:from_list(L);
to_map(M) when is_map(M)  -> M.


validate_options(L) when is_list(L) -> validate(L);
validate_options(L) when is_map(L)  -> validate(maps:to_list(L)).

validate([]) -> ok;
validate([{backlog_size, V} | T]) when is_integer(V) -> validate(T);
validate([{max_retries, V} | T]) when is_integer(V) -> validate(T);
validate([{pool_size, V} | T]) when is_integer(V) -> validate(T);
validate([{pool_strategy, random} | T]) -> validate(T);
validate([{pool_strategy, round_robin} | T]) -> validate(T);
validate([{pool_strategy, power_of_two} | T]) -> validate(T);
validate([{pool_strategy, least_connections} | T]) -> validate(T);
validate([{pool_strategy, random_without_replacement} | T]) -> validate(T);
validate([{pool_strategy, consistent_hash} | T]) -> validate(T);
validate([{ip, V} | T]) when is_list(V); is_tuple(V) -> validate(T);
validate([{address, V} | T]) when is_list(V); is_tuple(V) -> validate(T);
validate([{port, V} | T]) when is_integer(V) -> validate(T);
validate([{protocol, shackle_tcp} | T]) -> validate(T);
validate([{protocol, shackle_udp} | T]) -> validate(T);
validate([{protocol, shackle_ssl} | T]) -> validate(T);
validate([{service_name, V} | T]) when is_atom(V); is_binary(V); is_list(V) -> validate(T);
validate([{reconnect, V} | T]) when is_boolean(V) -> validate(T);
validate([{reconnect_time_max, V} | T]) when is_integer(V), V >= 0 -> validate(T);
validate([{reconnect_time_min, V} | T]) when is_integer(V), V >= 0 -> validate(T);
validate([{socket_options, V} | T]) when is_list(V) -> validate(T);
validate([{bounce_interval_secs, V} | T]) when is_integer(V) -> validate(T);
validate([{bounce_interval_secs, infinity} | T]) -> validate(T);
validate([{bounce_udp, V} | T]) when is_boolean(V) -> validate(T);
validate([{max_requests, V} | T]) when is_integer(V), V > 0 -> validate(T);
validate([{max_requests, infinity} | T]) -> validate(T);
validate([{on_bounce_event, {M,F}} | T]) when is_atom(M), is_atom(F) -> validate(T);
validate([{on_bounce_event, V} | T]) when is_function(V, 3) -> validate(T);
validate([{on_bounce_event, undefined} | T]) -> validate(T);
validate([KV | _]) -> erlang:error({invalid_shackle_option, KV}).

%%%----------------------------------------------------------------------------
%%% Testing
%%%----------------------------------------------------------------------------
-ifdef(EUNIT).

-define(A_PORT, 55447).
-define(AN_ADDRESS,
    "pacing-answerer-use1-dsp-prod-us-east-1b.pacing-answerer-prod.svc.cluster.local"
).
-define(SOME_DEFAULT_OPTIONS, #{
    pool_size => 1,
    port => 55447,
    max_retries => 3,
    backlog_size => 128,
    reconnect_time_min => timer:seconds(1),
    pool_strategy => round_robin,
    socket_options => ?SOME_DEFAULT_SOCKET_OPTIONS
}).
-define(SOME_DEFAULT_SOCKET_OPTIONS, [
    binary,
    {buffer, 65535},
    {nodelay, true},
    {packet, raw},
    {send_timeout, 50},
    {send_timeout_close, true}
]).
-define(AN_APPLICATION, pacingderl).

shackle_settings_hierarchy_test_() ->
    {setup,
        fun() ->
            with_env([
                {shackle, [{pool, [{pool_size, 100}]}]},
                {abcdef, [
                    {pool_size, 80},
                    {temp_pool_size, 70},
                    {shackle, [{pool, [{pool_size, 90}]}]}
                ]}
            ])
        end,
        fun cleanup/1,
        [
            % If there is an option name translation, use `temp_pool_size'
            ?_assertEqual(
                {[],[{pool_size, 70}]},
                shackle_utils:merge_options(
                    abcdef, [{pool_size, 10}], #{pool_size => temp_pool_size})),
            ?_assertEqual(ok, application:unset_env(abcdef, temp_pool_size)),

            % If there is an option name translation and no `temp_pool_size' is
            % defined, ignore the top-level `pool_size'.
            ?_assertEqual(
                {[],[{pool_size, 90}]},
                shackle_utils:merge_options(abcdef, [{pool_size, 10}], #{pool_size => temp_pool_size})),
            % If there is no option name translation, use top-level `pool_size'
            ?_assertEqual(
                {[],[{pool_size, 80}]},
                shackle_utils:merge_options(abcdef, [{pool_size, 10}])),
            ?_assertEqual(ok, application:unset_env(abcdef, pool_size)),

            ?_assertEqual(
                {[],[{pool_size, 90}]},
                shackle_utils:merge_options(
                    abcdef, [{pool_size, 10}], #{pool_size => temp_pool_size})),
            ?_assertEqual(ok, application:unset_env(abcdef, shackle)),

            ?_assertEqual(
                {[],[{pool_size, 100}]},
                shackle_utils:merge_options(
                    abcdef, [{pool_size, 10}], #{pool_size => temp_pool_size})),
            ?_assertEqual(ok, application:unset_env(shackle, pool)),

            ?_assertEqual(
                {[],[{pool_size, 10}]},
                shackle_utils:merge_options(
                    abcdef, [{pool_size, 10}], #{pool_size => temp_pool_size})),
            ?_assertEqual(
                {[],[{pool_size, 10}]},
                shackle_utils:merge_options(abcdef, [{pool_size, 10}])),

            ?_assertEqual({[],[]}, shackle_utils:merge_options(abcdef, []))
        ]
    }.

shackle_settings_test_() ->
    {setup,
        fun() ->
            with_env([
                {shackle, [{client, [{bounce_interval_secs, 100}]}]},
                {abcdef, [{bounce_interval_secs, 10}]}
            ])
        end,
        fun cleanup/1,
        [
            ?_assertEqual(10, check(shackle_utils:merge_options(abcdef, [{bounce_interval_secs, 300}]))),
            % Provided option is the same as shackle default value for this option,
            % but there's a global shackle value provided (100):
            ?_assertEqual(10, check(shackle_utils:merge_options(abcdef, [{bounce_interval_secs, infinity}]))),
            ?_assertEqual(ok, application:unset_env(shackle, client)),
            ?_assertEqual(10, check(shackle_utils:merge_options(abcdef, [{bounce_interval_secs, infinity}]))),
            ?_assertEqual(10, check(shackle_utils:merge_options(abcdef, []))),
            ?_assertEqual({[{bounce_interval_secs,10}], []},  shackle_utils:merge_options(abcdef, [])),
            ?_assertEqual(ok, application:set_env(abcdef, port, 20)),
            ?_assertEqual(20, check(port, shackle_utils:merge_options(abcdef, [])))
        ]
    }.

shackle_mapped_settings_test_() ->
    AppEnv = [{temp_interval_secs, 30}, {temp_port, 1234}],
    KeyMap = #{port => temp_port, bounce_interval_secs => temp_interval_secs},
    {setup,
        fun() ->
            with_env([{shackle, [{client, [{port, 10}, {bounce_interval_secs, 100}]}]}])
        end,
        fun cleanup/1,
        [
            ?_assertEqual(30, check(merge_options(AppEnv, [{bounce_interval_secs, 300}], KeyMap))),
            ?_assertEqual(1234, check(port, merge_options(AppEnv, [{port, 1000}], KeyMap))),
            ?_assertEqual({[{bounce_interval_secs,30},{port,1234}],[]},
                          merge_options(AppEnv, [{bounce_interval_secs, infinity}], KeyMap)),
            ?_assertEqual({[{bounce_interval_secs,30},{port,1234}],[{max_retries,5}]},
                          merge_options(AppEnv, [{max_retries, 5}], KeyMap)),
            ?_assertEqual({[{bounce_interval_secs,30},{port,1234}],[]}, merge_options(AppEnv, [], KeyMap))
        ]
    }.

shackle_validation_test_() ->
    {setup,
        fun() -> with_env([{shackle, [{client, [{port, a}]}]}]) end,
        fun cleanup/1,
        [
            ?_assertError({invalid_shackle_option, {port, a}}, merge_options([], [], #{})),
            ?_assertError({invalid_shackle_option, {max_restarts, 5}}, merge_options([], [{max_restarts, 5}], #{}))
        ]
    }.

ensure_top_value_is_used_even_if_equal_to_default_test_() ->
    {setup,
        fun() ->
            with_env([
                {?AN_APPLICATION, [
                    {backlog_size, ?DEFAULT_BACKLOG_SIZE},
                    {max_retries, ?DEFAULT_MAX_RETRIES},
                    {pool_size, ?DEFAULT_POOL_SIZE},
                    {pool_strategy, ?DEFAULT_POOL_STRATEGY},
                    {reconnect_time_min, 500},
                    {port, ?A_PORT},
                    {ip, ?AN_ADDRESS}
                ]}
            ])
        end,
        fun cleanup/1, [
            ?_assertEqual(
                {
                    [
                        {socket_options, ?SOME_DEFAULT_SOCKET_OPTIONS},
                        {reconnect_time_min, ?DEFAULT_RECONNECT_MIN},
                        {port, ?A_PORT},
                        {ip, ?AN_ADDRESS}
                    ],
                    [
                        {pool_strategy, ?DEFAULT_POOL_STRATEGY},
                        {pool_size, ?DEFAULT_POOL_SIZE},
                        {max_retries, ?DEFAULT_MAX_RETRIES},
                        {backlog_size, ?DEFAULT_BACKLOG_SIZE}
                    ]
                },
                shackle_utils:merge_options(?AN_APPLICATION, ?SOME_DEFAULT_OPTIONS)
            )
        ]}.

with_env(Values) ->
    lists:flatmap(
        fun({App, Keys}) ->
            lists:map(fun ({Key, New}) -> with_env(App, Key, New) end, Keys)
        end,
        Values
    ).

with_env(App, Key, New) ->
    Cleanup =
        case application:get_env(App, Key) of
            undefined ->
                fun() -> application:unset_env(App, Key) end;
            {ok, Old} ->
                fun() -> application:set_env(App, Key, Old) end
        end,
    application:set_env(App, Key, New),
    Cleanup.

cleanup(Cleanup) ->
    lists:foreach(fun(C) -> C() end, Cleanup).

check({L, _}) ->
    proplists:get_value(bounce_interval_secs, L, undefined).

check(I, {L, _}) ->
    proplists:get_value(I, L, undefined).

-endif.
