%% Regression tests for the findings in CODE_REVIEW.md.
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
        ?assert(is_boolean(try shackle_observe:enabled() catch C:R -> {C, R} end))
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
            Servers = [server_name(shackle_pool:server(?POOL_NAME_TCP)) || _ <- lists:seq(1, 100)],
            ?assert(length(lists:usort(Servers)) > 1)
        end)
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
            ignore(fun() -> shackle:receive_response(ReqId, 2000) end)
        end)
    end}.

%% Finding 6: a servers list of {Host, Port} entries needs no top-level port
servers_without_port_test_() ->
    {timeout, 15, fun () ->
        Cleanup = shackle_tests:setup(?MODULE),
        ok = arithmetic_tcp_server:start(),
        try
            ?assertEqual(ok, try shackle_pool:start(?POOL_NAME_TCP, ?CLIENT_TCP, [
                {servers, [{"127.0.0.1", ?PORT}]},
                {reconnect_time_min, 1},
                {socket_options, [binary, {packet, raw}]}
            ], [{pool_size, 1}]) catch C:R -> {C, R} end),
            ?assert(arithmetic_tcp_client:wait_until_all_available(4000)),
            ?assertEqual(2, arithmetic_tcp_client:add(1, 1))
        after
            ignore(fun() -> shackle_pool:stop(?POOL_NAME_TCP) end),
            ignore(fun() -> arithmetic_tcp_server:stop() end),
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

%% callbacks
bounce_event(PoolName, Index, Event) ->
    ignore(fun() -> ?BOUNCE_EVENTS ! {bounce_event, PoolName, Index, Event} end),
    ok.

%% utils
%% Run Fun for its side effect; any failure is ignored (cleanup and discarded results).
ignore(Fun) ->
    try Fun() catch _:_ -> ok end.

flush() ->
    receive _ -> flush()
    after 0 -> ok
    end.

server_name({ok, _Client, ServerName, _Sema}) -> ServerName;
server_name(Error) -> Error.

with_pool(Client, PoolOptions, ClientOptions, Fun) ->
    Cleanup = shackle_tests:setup(?MODULE, Client, PoolOptions, ClientOptions),
    try
        Fun()
    after
        shackle_tests:cleanup(Client, Cleanup)
    end.
