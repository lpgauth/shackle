-module(shackle_tests).
-include_lib("eunit/include/eunit.hrl").
-include("test.hrl").
-include("shackle_defaults.hrl").

-export([server/1, setup/0, setup/1, setup/2, setup/3, setup/4]).
-export([cleanup/1, cleanup/2]).

-define(N, 50).
-define(LONG_TEST_BACKLOG_SIZE, 10_000).
-define(LONG_TEST_TIMEOUT, 10).
-define(LONG_TEST_POOL_SIZE, 16).

%% runners
shackle_app_stop_start_test_() ->
    {setup,
        fun () ->
            Cleanup = setup(),
            ?CLIENT_TCP:start(),
            Cleanup
        end,
        fun (Cleanup) -> cleanup(?CLIENT_TCP, Cleanup) end,
    [fun app_stop_start_subtest/0]}.

shackle_backlog_test_() ->
    {setup,
        fun () ->
            setup(?CLIENT_TCP, [
                {backlog_size, 1},
                {pool_size, 1}
            ])
        end,
        fun (Cleanup) -> cleanup(?CLIENT_TCP, Cleanup) end,
    [fun backlog_full_subtest/0]}.

shackle_backlog_infinity_test_() ->
    {setup,
        fun () ->
            setup(?CLIENT_TCP, [
                {backlog_size, infinity},
                {pool_size, 1}
            ])
        end,
        fun (Cleanup) -> cleanup(?CLIENT_TCP, Cleanup) end,
    [fun () -> add_subtest(?CLIENT_TCP) end]}.

shackle_call_crash_test_() ->
    {setup,
        fun () ->
            setup(?CLIENT_TCP, [
                {pool_size, 1}
            ])
        end,
        fun (Cleanup) -> cleanup(?CLIENT_TCP, Cleanup) end,
    [fun call_crash_subtest/0]}.

shackle_random_ssl_test_() ->
    {setup,
        fun () ->
            setup(?CLIENT_SSL, [
                {pool_size, 1},
                {pool_strategy, random}
        ]) end,
        fun (Cleanup) -> cleanup(?CLIENT_SSL, Cleanup) end,
    {inparallel, [
        fun () -> add_subtest(?CLIENT_SSL) end,
        fun () -> multiply_subtest(?CLIENT_SSL) end,
        fun () -> noop_subtest(?CLIENT_SSL) end
    ]}}.

shackle_random_tcp_test_() ->
    {setup,
        fun () ->
            setup(?CLIENT_TCP, [
                {pool_size, 1},
                {pool_strategy, random}
            ])
        end,
        fun (Cleanup) -> cleanup(?CLIENT_TCP, Cleanup) end,
    {inparallel, [
        fun () -> add_subtest(?CLIENT_TCP) end,
        fun () -> multiply_subtest(?CLIENT_TCP) end,
        fun () -> noop_subtest(?CLIENT_TCP) end
    ]}}.

shackle_random_udp_test_() ->
    {setup,
        fun () ->
            setup(?CLIENT_UDP, [
                {pool_size, 1},
                {pool_strategy, random}
            ])
        end,
        fun (Cleanup) -> cleanup(?CLIENT_UDP, Cleanup) end,
    {inparallel, [
        fun () -> add_subtest(?CLIENT_UDP) end,
        fun () -> multiply_subtest(?CLIENT_UDP) end,
        fun () -> noop_subtest(?CLIENT_UDP) end
    ]}}.

shackle_reconnect_ssl_test_() ->
    {setup,
        fun () -> setup() end,
        fun (Cleanup) -> cleanup(?CLIENT_SSL, Cleanup) end,
    [fun () -> reconnect_subtest(?CLIENT_SSL) end]}.

shackle_reconnect_tcp_test_() ->
    {setup,
        fun () -> setup() end,
        fun (Cleanup) -> cleanup(?CLIENT_TCP, Cleanup) end,
    [fun () -> reconnect_subtest(?CLIENT_TCP) end]}.

shackle_reconnect_udp_test_() ->
    {setup,
        fun () -> setup() end,
        fun (Cleanup) -> cleanup(?CLIENT_UDP, Cleanup) end,
    [fun () -> reconnect_subtest(?CLIENT_UDP) end]}.

shackle_round_robin_tcp_test_() ->
    {setup,
        fun () -> setup(?CLIENT_TCP, [{pool_strategy, round_robin}]) end,
        fun (Cleanup) -> cleanup(?CLIENT_TCP, Cleanup) end,
    {inparallel, [
        fun () -> add_subtest(?CLIENT_TCP) end,
        fun () -> multiply_subtest(?CLIENT_TCP) end,
        fun () -> noop_subtest(?CLIENT_TCP) end
    ]}}.

shackle_round_robin_udp_test_() ->
    {setup,
        fun () -> setup(?CLIENT_UDP, [{pool_strategy, round_robin}]) end,
        fun (Cleanup) -> cleanup(?CLIENT_UDP, Cleanup) end,
    {inparallel, [
        fun () -> add_subtest(?CLIENT_UDP) end,
        fun () -> multiply_subtest(?CLIENT_UDP) end,
        fun () -> noop_subtest(?CLIENT_UDP) end
    ]}}.

shackle_power_of_two_tcp_test_() ->
    {setup,
        fun () -> setup(?CLIENT_TCP, [{pool_strategy, power_of_two}]) end,
        fun (Cleanup) -> cleanup(?CLIENT_TCP, Cleanup) end,
    {inparallel, [
        fun () -> add_subtest(?CLIENT_TCP) end,
        fun () -> multiply_subtest(?CLIENT_TCP) end,
        fun () -> noop_subtest(?CLIENT_TCP) end
    ]}}.

shackle_power_of_two_udp_test_() ->
    {setup,
        fun () -> setup(?CLIENT_UDP, [{pool_strategy, power_of_two}]) end,
        fun (Cleanup) -> cleanup(?CLIENT_UDP, Cleanup) end,
    {inparallel, [
        fun () -> add_subtest(?CLIENT_UDP) end,
        fun () -> multiply_subtest(?CLIENT_UDP) end,
        fun () -> noop_subtest(?CLIENT_UDP) end
    ]}}.

shackle_least_connections_tcp_test_() ->
    {setup,
        fun () -> setup(?CLIENT_TCP, [{pool_strategy, least_connections}]) end,
        fun (Cleanup) -> cleanup(?CLIENT_TCP, Cleanup) end,
    {inparallel, [
        fun () -> add_subtest(?CLIENT_TCP) end,
        fun () -> multiply_subtest(?CLIENT_TCP) end,
        fun () -> noop_subtest(?CLIENT_TCP) end
    ]}}.

shackle_least_connections_udp_test_() ->
    {setup,
        fun () -> setup(?CLIENT_UDP, [{pool_strategy, least_connections}]) end,
        fun (Cleanup) -> cleanup(?CLIENT_UDP, Cleanup) end,
    {inparallel, [
        fun () -> add_subtest(?CLIENT_UDP) end,
        fun () -> multiply_subtest(?CLIENT_UDP) end,
        fun () -> noop_subtest(?CLIENT_UDP) end
    ]}}.

shackle_random_without_replacement_tcp_test_() ->
    {setup,
        fun () -> setup(?CLIENT_TCP, [{pool_strategy, random_without_replacement}]) end,
        fun (Cleanup) -> cleanup(?CLIENT_TCP, Cleanup) end,
    {inparallel, [
        fun () -> add_subtest(?CLIENT_TCP) end,
        fun () -> multiply_subtest(?CLIENT_TCP) end,
        fun () -> noop_subtest(?CLIENT_TCP) end
    ]}}.

shackle_random_without_replacement_udp_test_() ->
    {setup,
        fun () -> setup(?CLIENT_UDP, [{pool_strategy, random_without_replacement}]) end,
        fun (Cleanup) -> cleanup(?CLIENT_UDP, Cleanup) end,
    {inparallel, [
        fun () -> add_subtest(?CLIENT_UDP) end,
        fun () -> multiply_subtest(?CLIENT_UDP) end,
        fun () -> noop_subtest(?CLIENT_UDP) end
    ]}}.

shackle_consistent_hash_tcp_test_() ->
    {setup,
        fun () -> setup(?CLIENT_TCP, [{pool_strategy, consistent_hash}]) end,
        fun (Cleanup) -> cleanup(?CLIENT_TCP, Cleanup) end,
    {inparallel, [
        fun () -> add_subtest(?CLIENT_TCP) end,
        fun () -> multiply_subtest(?CLIENT_TCP) end,
        fun () -> noop_subtest(?CLIENT_TCP) end
    ]}}.

shackle_consistent_hash_udp_test_() ->
    {setup,
        fun () -> setup(?CLIENT_UDP, [{pool_strategy, consistent_hash}]) end,
        fun (Cleanup) -> cleanup(?CLIENT_UDP, Cleanup) end,
    {inparallel, [
        fun () -> add_subtest(?CLIENT_UDP) end,
        fun () -> multiply_subtest(?CLIENT_UDP) end,
        fun () -> noop_subtest(?CLIENT_UDP) end
    ]}}.

shackle_timeout_ssl_test_() ->
    {setup,
        fun () ->
            setup(?CLIENT_SSL, [{pool_size, 1}, {pool_strategy, random}])
        end,
        fun (Cleanup) -> cleanup(?CLIENT_SSL, Cleanup) end,
    [fun () -> timeout_subtest(?CLIENT_SSL) end]}.

shackle_timeout_tcp_test_() ->
    {setup,
        fun () ->
            setup(?CLIENT_TCP, [{pool_size, 1}, {pool_strategy, random}])
        end,
        fun (Cleanup) -> cleanup(?CLIENT_TCP, Cleanup) end,
    [fun () -> timeout_subtest(?CLIENT_TCP) end]}.

shackle_timeout_udp_test_() ->
    {setup,
        fun () ->
            setup(?CLIENT_UDP, [{pool_size, 1}, {pool_strategy, random}])
        end,
        fun (Cleanup) -> cleanup(?CLIENT_UDP, Cleanup) end,
    [fun () -> timeout_subtest(?CLIENT_UDP) end]}.

shackle_modulo_test_() ->
    {setup,
        fun () ->
            setup(?CLIENT_TCP, [{pool_size, 1}, {pool_strategy, random}])
        end,
        fun (Cleanup) -> cleanup(?CLIENT_TCP, Cleanup) end,
        [
            fun () -> modulo_with_return_value(?CLIENT_TCP) end,
            fun () -> modulo_without_return_value(?CLIENT_TCP) end
        ]}.

%% tests
add_subtest(Client) ->
    [assert_random_add(Client) || _ <- lists:seq(1, ?N)].

app_stop_start_subtest() ->
    ?assertEqual({error, no_server}, arithmetic_tcp_client:add(1, 1)),
    ok = arithmetic_tcp_server:start(),
    true = arithmetic_tcp_client:wait_until_all_available(5000),
    ?assertEqual(2, arithmetic_tcp_client:add(1, 1)),
    ok = arithmetic_tcp_client:stop(),

    shackle_app:stop(),
    shackle_app:start(),

    arithmetic_tcp_client:start(),
    true = arithmetic_tcp_client:wait_until_all_available(5000),
    ?assertEqual(2, arithmetic_tcp_client:add(1, 1)).

backlog_full_subtest() ->
    Pid = self(),
    [spawn(fun () ->
        Pid ! {response, arithmetic_tcp_client:add(1, 1)}
    end) || _ <- lists:seq(1, 20)],

    ?assert([ok || {error, no_server} <- receive_loop(20)] /= []).

call_crash_subtest() ->
    ?assertEqual({error, client_crash}, arithmetic_tcp_client:add(a, b)),
    ?assertEqual(2, arithmetic_tcp_client:add(1, 1)).

multiply_subtest(Client) ->
    [assert_random_multiply(Client) || _ <- lists:seq(1, ?N)].

noop_subtest(Client) ->
    [Client:noop() || _ <- lists:seq(1, 10)].

reconnect_subtest(Client) ->
    Server = server(Client),
    Client:start([
        {pool_size, 1},
        {pool_strategy, random}
    ]),
    ?assertEqual({error, no_server}, Client:add(1, 1)),
    ok = Server:start(),
    ?assert(Client:wait_until_all_available(2000)),
    ?assertEqual(2, Client:add(1, 1)),
    ok = Server:stop(),
    timer:sleep(100),
    {error, _} = Client:add(1, 1),

    %ok = Server:start(),
    %timer:sleep(100),
    %?assertEqual(2, Client:add(1, 1)).
    ok.

timeout_subtest(Client) ->
    ?assertEqual({error, timeout_handled}, Client:add(255, 255)).

modulo_with_return_value(Client) ->
    V = Client:modulo(5, 3),
    ?assertEqual(5 rem 3, V).

modulo_without_return_value(Client) ->
    V = Client:modulo(5, 0),
    ?assertEqual({error, timeout_handled}, V).

%% utils
assert_random_add(Client) ->
    A = rand(),
    B = rand(),
    Res = Client:add(A, B),
    ?assertEqual(A + B, Res).

assert_random_multiply(Client) ->
    A = rand(),
    B = rand(),
    ?assertEqual(A * B, Client:multiply(A, B)).

rand() ->
    shackle_utils:random(254).

receive_loop(0) ->
    [];
receive_loop(N) ->
    receive
        {response, X} ->
            [X | receive_loop(N - 1)]
    end.

server(?CLIENT_SSL) -> arithmetic_ssl_server;
server(?CLIENT_TCP) -> arithmetic_tcp_server;
server(?CLIENT_UDP) -> arithmetic_udp_server.

setup() ->
    setup(?MODULE).

setup(_Module) ->
    error_logger:tty(false),
    Cleanup = shackle_test_utils:with_prometheus(),
    shackle_app:start(),
    % Disable OTP supervisor reports
    %logger:add_handler_filter(default, Module, {fun
    %    (#{meta := #{error_logger := #{tag := info_report}}}, _) -> stop;
    %    (_, _) -> log
    %end, nostate}),
    Cleanup.

setup(Client, PoolOptions) ->
    setup(?MODULE, Client, PoolOptions).

setup(Module, Client, PoolOptions) ->
    setup(Module, Client, PoolOptions, []).

setup(Module, Client, PoolOptions, SrvOptions) ->
    Cleanup = setup(Module),
    Server = server(Client),
    Server:start(),
    Client:start(PoolOptions, SrvOptions),
    Client:wait_until_all_available(4000) orelse
        erlang:error({server_not_available, Client}),
    Cleanup.

cleanup(Cleanup) ->
    try shackle_app:stop() catch _:_ -> ok end,
    logger:remove_handler_filter(default, ?MODULE),
    shackle_test_utils:cleanup_mocks(Cleanup).

cleanup(Client, Cleanup) ->
    try Client:stop() catch _:_ -> ok end,
    Server = server(Client),
    try Server:stop() catch _:_ -> ok end,
    cleanup(Cleanup).
