%% Regression tests for the findings in CODE_REVIEW.md and CODE_REVIEW_2.md.
%%
%% Every test asserts the *correct* behaviour, so each one fails until the
%% corresponding finding is fixed.
-module(shackle_review_findings_tests).
-include_lib("eunit/include/eunit.hrl").
-include("test.hrl").

-export([bounce_event/3]).

-define(BOUNCE_EVENTS, shackle_review_bounce_events).

%% Finding 1: tuple observability config must not crash backend/0
observe_tuple_config_test() ->
    OldEnv = application:get_env(shackle, observability),
    persistent_term:erase(shackle_observe),
    application:set_env(shackle, observability, {prometheus, #{}}),
    try
        ?assert(is_boolean(catch shackle_observe:enabled()))
    after
        persistent_term:erase(shackle_observe),
        case OldEnv of
            {ok, V}   -> application:set_env(shackle, observability, V);
            undefined -> application:unset_env(shackle, observability)
        end
    end.

%% Finding 2: a connection draining after max_requests must leave the pool
max_requests_drain_disables_server_test_() ->
    {timeout, 15, fun () ->
        with_pool(?CLIENT_TCP, [{pool_size, 1}], [{max_requests, 1}], fun () ->
            {ok, ReqId} = shackle:cast(?POOL_NAME_TCP, {delayed_echo, 500}, self(), 2000),
            timer:sleep(100),
            ?assertNot(shackle_status:active({?POOL_NAME_TCP, 1})),
            shackle:receive_response(ReqId, 2000)
        end)
    end}.

%% Finding 3: least_connections must not pin all traffic to one connection
least_connections_spreads_load_test_() ->
    {timeout, 15, fun () ->
        PoolOpts = [
            {pool_size, 4},
            {pool_strategy, least_connections}
        ],
        with_pool(?CLIENT_TCP, PoolOpts, [], fun () ->
            Servers = [select_and_release(?POOL_NAME_TCP) || _ <- lists:seq(1, 100)],
            ?assert(length(lists:usort(Servers)) > 1)
        end)
    end}.

least_connections_rejects_infinite_backlog_test_() ->
    {timeout, 15, fun () ->
        Cleanup = shackle_tests:setup(?MODULE),
        try
            ?assertError({incompatible_pool_options, _, _},
                arithmetic_tcp_client:start([
                    {pool_size, 4},
                    {pool_strategy, least_connections},
                    {backlog_size, infinity}
                ]))
        after
            catch shackle_pool:stop(?POOL_NAME_TCP),
            shackle_tests:cleanup(Cleanup)
        end
    end}.

%% Finding 4: call/2 must wait for the server-side timeout and leave no
%% stray reply in the caller's mailbox
call2_no_stray_reply_test_() ->
    {timeout, 15, fun () ->
        with_pool(?CLIENT_TCP, [{pool_size, 1}], [], fun () ->
            flush(),
            Reply = shackle:call(?POOL_NAME_TCP, {delayed_echo, 1200}),
            timer:sleep(500),
            ?assertEqual({messages, []}, process_info(self(), messages)),
            ?assertEqual({error, timeout_handled}, Reply)
        end)
    end}.

%% Finding 5: receive_response/2 must keep raising error:timeout
receive_response_raises_on_timeout_test_() ->
    {timeout, 15, fun () ->
        with_pool(?CLIENT_TCP, [{pool_size, 1}], [], fun () ->
            {ok, ReqId} = shackle:cast(?POOL_NAME_TCP, {delayed_echo, 300}, self(), 2000),
            ?assertError(timeout, shackle:receive_response(ReqId, 50)),
            catch shackle:receive_response(ReqId, 2000)
        end)
    end}.

%% Finding 6: a servers list of {Host, Port} entries needs no top-level port
servers_without_port_test_() ->
    {timeout, 15, fun () ->
        Cleanup = shackle_tests:setup(?MODULE),
        ok = arithmetic_tcp_server:start(),
        try
            ?assertEqual(ok, catch shackle_pool:start(?POOL_NAME_TCP, ?CLIENT_TCP, [
                {servers, [{"127.0.0.1", ?PORT}]},
                {reconnect_time_min, 1},
                {socket_options, [binary, {packet, raw}]}
            ], [{pool_size, 1}])),
            ?assert(arithmetic_tcp_client:wait_until_all_available(4000)),
            ?assertEqual(2, arithmetic_tcp_client:add(1, 1))
        after
            catch shackle_pool:stop(?POOL_NAME_TCP),
            catch arithmetic_tcp_server:stop(),
            shackle_tests:cleanup(Cleanup)
        end
    end}.

%% Finding 7: short names are Kubernetes candidates, and names under the
%% cluster domain without ".svc." are not expanded with search domains
k8s_short_name_detected_test() ->
    ?assert(shackle_servers:is_short_name("my-svc")).

k8s_pod_name_not_expanded_test() ->
    ?assertNot(shackle_servers:is_short_name("10-0-0-1.my-ns.pod.cluster.local")).

%% Finding 8: random_without_replacement must not retry the same server
random_without_replacement_no_repeat_test_() ->
    {timeout, 15, fun () ->
        PoolOpts = [
            {pool_size, 2},
            {max_retries, 1},
            {pool_strategy, random_without_replacement},
            {backlog_size, infinity}
        ],
        with_pool(?CLIENT_TCP, PoolOpts, [], fun () ->
            shackle_status:disable({?POOL_NAME_TCP, 1}),
            Servers = [server_name(shackle_pool:server(?POOL_NAME_TCP)) || _ <- lists:seq(1, 200)],
            ?assertEqual([arithmetic_tcp_2], lists:usort(Servers))
        end)
    end}.

%% Pre-existing: UDP connections are not bounced unless bounce_udp is true
bounce_udp_default_test_() ->
    {timeout, 15, fun () ->
        with_pool(?CLIENT_UDP, [{pool_size, 1}], [{bounce_interval_secs, 60}], fun () ->
            ?assertEqual({ok, infinity}, shackle_server:next_bounce(?POOL_NAME_UDP, 1))
        end)
    end}.

%% Pre-existing: an {M, F} on_bounce_event callback is invoked
on_bounce_event_mf_test_() ->
    {timeout, 15, fun () ->
        true = register(?BOUNCE_EVENTS, self()),
        try
            with_pool(?CLIENT_TCP, [{pool_size, 1}], [{on_bounce_event, {?MODULE, bounce_event}}], fun () ->
                receive
                    {bounce_event, ?POOL_NAME_TCP, 1, #{status := connected}} -> ok
                after 2000 ->
                    ?assert(false)
                end
            end)
        after
            unregister(?BOUNCE_EVENTS)
        end
    end}.

%% Pre-existing: a timeout of infinity must not crash the connection process
call_infinity_timeout_test_() ->
    {timeout, 15, fun () ->
        with_pool(?CLIENT_TCP, [{pool_size, 1}], [], fun () ->
            ?assertEqual(2, shackle:call(?POOL_NAME_TCP, {add, 1, 1}, infinity, 1000))
        end)
    end}.

%% CODE_REVIEW_2.md findings

%% Review 2, finding 1: shackle_socket must deliver received data to its owner
socket_protocol_delivers_data_test_() ->
    {timeout, 15, fun () ->
        with_tcp_server(fun () ->
            {ok, S} = shackle_socket:connect({127, 0, 0, 1}, ?PORT, [binary, {packet, raw}]),
            try
                %% setopts active arms reads; data arrives as select notifications
                ok = shackle_socket:setopts(S, [{active, true}]),
                ok = shackle_socket:send(S, <<"INIT">>),
                ?assertEqual(<<"OK">>, recv_socket(S, 1000)),
                ok = shackle_socket:send(S, arithmetic_protocol:request(1, add, 1, 1)),
                ?assertEqual(<<1:32/integer, 2:16/integer>>, recv_socket(S, 1000))
            after
                shackle_socket:close(S)
            end
        end)
    end}.

%% Review 2, finding 2: shackle_ssl_socket must apply non-TLS socket options
ssl_socket_protocol_keeps_options_test_() ->
    {timeout, 15, fun () ->
        Cleanup = shackle_tests:setup(?MODULE),
        ok = arithmetic_ssl_server:start(),
        try
            {ok, S} = shackle_ssl_socket:connect({127, 0, 0, 1}, ?PORT,
                [binary, {packet, raw}, {nodelay, true}, {verify, verify_none}]),
            try
                {ok, Opts} = ssl:getopts(S, [mode, nodelay]),
                ?assertEqual([{mode, binary}, {nodelay, true}], lists:sort(Opts))
            after
                shackle_ssl_socket:close(S)
            end
        after
            catch arithmetic_ssl_server:stop(),
            shackle_tests:cleanup(Cleanup)
        end
    end}.

%% Review 2, finding 3: covered by servers_without_port_test_ above

%% Review 2, finding 4: a request that never gets a reply must not stall a
%% max_requests drain forever
infinite_timeout_drain_completes_test_() ->
    {timeout, 30, fun () ->
        with_pool(?CLIENT_TCP, [{pool_size, 1}], [{max_requests, 1}], fun () ->
            %% modulo by 0 is never answered by the test server
            {ok, _ReqId} = shackle:cast(?POOL_NAME_TCP, {modulo, 5, 0}, self(), #{timeout => infinity}),
            timer:sleep(100),
            ?assertNot(shackle_status:active({?POOL_NAME_TCP, 1})),
            ?assert(wait_until(fun () -> shackle_status:active({?POOL_NAME_TCP, 1}) end, 10000))
        end)
    end}.

%% Review 2, finding 5: shackle_socket:send/2 must succeed for a flat binary
%% (socket:sendv/2 returns plain `ok' on success, which send/2 does not match)
socket_protocol_sends_binary_test_() ->
    {timeout, 15, fun () ->
        with_tcp_server(fun () ->
            {ok, S} = shackle_socket:connect({127, 0, 0, 1}, ?PORT, [binary]),
            try
                ?assertEqual(ok, shackle_socket:send(S, arithmetic_protocol:request(1, add, 1, 1)))
            after
                shackle_socket:close(S)
            end
        end)
    end}.

%% Review 2, finding 5: shackle_socket:send/2 must accept nested iodata
socket_protocol_sends_iodata_test_() ->
    {timeout, 15, fun () ->
        with_tcp_server(fun () ->
            {ok, S} = shackle_socket:connect({127, 0, 0, 1}, ?PORT, [binary]),
            try
                ?assertEqual(ok, shackle_socket:send(S, [<<1:32/integer>>, [<<1>>, "a"], <<1>>]))
            after
                shackle_socket:close(S)
            end
        end)
    end}.

%% Review 2, finding 6: consistent_hash retries must try another connection
%% when the key's connection is unavailable
consistent_hash_retries_other_server_test_() ->
    {timeout, 15, fun () ->
        PoolOpts = [
            {pool_size, 4},
            {max_retries, 3},
            {pool_strategy, consistent_hash}
        ],
        with_pool(?CLIENT_TCP, PoolOpts, [], fun () ->
            Key = <<"session-1">>,
            shackle_status:disable({?POOL_NAME_TCP, erlang:phash2(Key, 4) + 1}),
            ?assertMatch({ok, _, _, _}, shackle_pool:server(?POOL_NAME_TCP, #{hash_key => Key}))
        end)
    end}.

%% Review 2, finding 7: power_of_two must not prefer inactive servers
power_of_two_skips_inactive_servers_test_() ->
    {timeout, 15, fun () ->
        PoolOpts = [
            {pool_size, 16},
            {max_retries, 3},
            {pool_strategy, power_of_two}
        ],
        with_pool(?CLIENT_TCP, PoolOpts, [], fun () ->
            %% servers 1-8 are down, servers 9-16 each have a request in flight
            [shackle_status:disable({?POOL_NAME_TCP, I}) || I <- lists:seq(1, 8)],
            [{ok, _} = shackle_sema:acquire(?POOL_NAME_TCP, I) || I <- lists:seq(9, 16)],
            Results = [select_and_release(?POOL_NAME_TCP) || _ <- lists:seq(1, 1000)],
            NoServer = length([R || {error, no_server} = R <- Results]),
            %% random would fail ~6.25% of calls here; power_of_two should do no worse
            ?assert(NoServer < 100)
        end)
    end}.

%% Review 2, finding 8: merge_options must accept and keep the servers option
merge_options_servers_test() ->
    Servers = [{"a", 1}],
    {ClientOpts, _PoolOpts} = shackle_utils:merge_options(shackle_review_no_app,
        [{servers, Servers}, {pool_size, 2}]),
    ?assertEqual(Servers, proplists:get_value(servers, ClientOpts)).

%% Review 2, finding 8: servers set only in the app env must be picked up
merge_options_servers_app_env_test() ->
    application:set_env(shackle_review_app, servers, [{"b", 2}]),
    try
        {ClientOpts, _} = shackle_utils:merge_options(shackle_review_app, [{pool_size, 2}]),
        ?assertEqual([{"b", 2}], proplists:get_value(servers, ClientOpts))
    after
        application:unset_env(shackle_review_app, servers)
    end.

%% Waits for a socket-module connection to deliver one chunk of data.
%% recv/1 re-arms the select after each read, so the loop only ends on `select'.
recv_socket(S, Timeout) ->
    receive
        {'$socket', S, select, _} ->
            case shackle_socket:recv(S) of
                {ok, Data} -> rearm(S), Data;
                wait -> recv_socket(S, Timeout)
            end
    after Timeout ->
        timeout
    end.

rearm(S) ->
    case shackle_socket:recv(S) of
        {ok, _} -> rearm(S);
        wait -> ok
    end.

%% callbacks
bounce_event(PoolName, Index, Event) ->
    catch ?BOUNCE_EVENTS ! {bounce_event, PoolName, Index, Event},
    ok.

%% utils
flush() ->
    receive _ -> flush()
    after 0 -> ok
    end.

server_name({ok, _Client, ServerName, _Sema}) -> ServerName;
server_name(Error) -> Error.

%% Select a server and give its backlog slot back, so every selection sees an
%% idle pool and only the tie-break decides
select_and_release(PoolName) ->
    case shackle_pool:server(PoolName) of
        {ok, _Client, ServerName, Sema} ->
            shackle_sema:release(Sema, self()),
            ServerName;
        Error ->
            Error
    end.

wait_until(Fun, Timeout) when Timeout =< 0 ->
    Fun();
wait_until(Fun, Timeout) ->
    case Fun() of
        true ->
            true;
        false ->
            timer:sleep(100),
            wait_until(Fun, Timeout - 100)
    end.

with_tcp_server(Fun) ->
    Cleanup = shackle_tests:setup(?MODULE),
    ok = arithmetic_tcp_server:start(),
    try
        Fun()
    after
        catch arithmetic_tcp_server:stop(),
        shackle_tests:cleanup(Cleanup)
    end.

with_pool(Client, PoolOptions, ClientOptions, Fun) ->
    Cleanup = shackle_tests:setup(?MODULE, Client, PoolOptions, ClientOptions),
    try
        Fun()
    after
        shackle_tests:cleanup(Client, Cleanup)
    end.
