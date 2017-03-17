-module(shackle_tests).
-include_lib("eunit/include/eunit.hrl").
-include("test.hrl").

-define(N, 1000).

%% runners
shackle_backlog_test_() ->
    {setup,
        fun () ->
            setup_tcp([
                {backlog_size, 1},
                {pool_size, 1}
            ])
        end,
        fun (_) -> cleanup_tcp() end,
    [fun backlog_full_subtest/0]}.

shackle_backlog_infinity_test_() ->
    {setup,
        fun () ->
            setup_tcp([
                {backlog_size, infinity},
                {pool_size, 1}
            ])
        end,
        fun (_) -> cleanup_tcp() end,
    [fun add_tcp_subtest/0]}.

shackle_random_ssl_test_() ->
    {setup,
        fun () -> setup_ssl([
            {pool_size, 1},
            {pool_strategy, random}
        ]) end,
        fun (_) -> cleanup_ssl() end,
    {inparallel, [
        fun add_ssl_subtest/0,
        fun multiply_ssl_subtest/0
    ]}}.

shackle_random_tcp_test_() ->
    {setup,
        fun () -> setup_tcp([
            {pool_size, 1},
            {pool_strategy, random}
        ]) end,
        fun (_) -> cleanup_tcp() end,
    {inparallel, [
        fun add_tcp_subtest/0,
        fun multiply_tcp_subtest/0
    ]}}.

shackle_random_udp_test_() ->
    {setup,
        fun () -> setup_udp([
            {pool_size, 1},
            {pool_strategy, random}
        ]) end,
        fun (_) -> cleanup_udp() end,
    {inparallel, [
        fun add_udp_subtest/0,
        fun multiply_udp_subtest/0
    ]}}.

shackle_reconnect_ssl_test_() ->
    {setup,
        fun () ->
            setup(),
            shackle_pool:start(?POOL_NAME, ?CLIENT_SSL, [
                {port, ?PORT},
                {protocol, shackle_ssl},
                {reconnect, true},
                {reconnect_time_min, 1},
                {socket_options, [binary, {packet, raw}]}
            ], [{pool_size, 1}])
        end,
        fun (_) -> cleanup_ssl() end,
    [fun reconnect_ssl_subtest/0]}.

shackle_reconnect_tcp_test_() ->
    {setup,
        fun () ->
            setup(),
            shackle_pool:start(?POOL_NAME, ?CLIENT_TCP, [
                {port, ?PORT},
                {protocol, shackle_tcp},
                {reconnect, true},
                {reconnect_time_min, 1},
                {socket_options, [binary, {packet, raw}]}
            ], [{pool_size, 1}])
        end,
        fun (_) -> cleanup_tcp() end,
    [fun reconnect_tcp_subtest/0]}.

shackle_reconnect_udp_test_() ->
    {setup,
        fun () ->
            setup(),
            shackle_pool:start(?POOL_NAME, ?CLIENT_UDP, [
                {port, ?PORT},
                {protocol, shackle_udp},
                {reconnect, true},
                {reconnect_time_min, 1},
                {socket_options, [binary]
            }], [{pool_size, 1}])
        end,
        fun (_) -> cleanup_tcp() end,
    [fun reconnect_udp_subtest/0]}.

shackle_round_robin_tcp_test_() ->
    {setup,
        fun () -> setup_tcp([
            {pool_strategy, round_robin}
        ]) end,
        fun (_) -> cleanup_tcp() end,
    {inparallel, [
        fun add_tcp_subtest/0,
        fun multiply_tcp_subtest/0
    ]}}.

shackle_round_robin_udp_test_() ->
    {setup,
        fun () -> setup_udp([
            {pool_strategy, round_robin}
        ]) end,
        fun (_) -> cleanup_udp() end,
    {inparallel, [
        fun add_udp_subtest/0,
        fun multiply_udp_subtest/0
    ]}}.

%% tests
add_ssl_subtest() ->
    [assert_random_add(?CLIENT_SSL) || _ <- lists:seq(1, ?N)].

add_tcp_subtest() ->
    [assert_random_add(?CLIENT_TCP) || _ <- lists:seq(1, ?N)].

add_udp_subtest() ->
    [assert_random_add(?CLIENT_UDP) || _ <- lists:seq(1, ?N)].

backlog_full_subtest() ->
    Pid = self(),
    [spawn(fun () ->
        X = arithmetic_tcp_client:add(1, 1),
        Pid ! {response, X}
    end) || _ <- lists:seq(1, 20)],

    ?assert(lists:any(fun
        ({error, backlog_full}) -> true;
        (_) -> false
    end, receive_loop(20))).

multiply_ssl_subtest() ->
    [assert_random_multiply(?CLIENT_SSL) || _ <- lists:seq(1, ?N)].

multiply_tcp_subtest() ->
    [assert_random_multiply(?CLIENT_TCP) || _ <- lists:seq(1, ?N)].

multiply_udp_subtest() ->
    [assert_random_multiply(?CLIENT_UDP) || _ <- lists:seq(1, ?N)].

reconnect_ssl_subtest() ->
    ?assertEqual({error, no_socket}, arithmetic_ssl_client:add(1, 1)),
    ok = arithmetic_ssl_server:start(),
    timer:sleep(100),
    ?assertEqual(2, arithmetic_ssl_client:add(1, 1)),
    ok = arithmetic_ssl_server:stop(),
    timer:sleep(100),
    ?assertEqual({error, no_socket}, arithmetic_ssl_client:add(1, 1)),
    ok = arithmetic_ssl_server:start(),
    timer:sleep(100),
    ?assertEqual(2, arithmetic_ssl_client:add(1, 1)).

reconnect_tcp_subtest() ->
    ?assertEqual({error, no_socket}, arithmetic_tcp_client:add(1, 1)),
    ok = arithmetic_tcp_server:start(),
    timer:sleep(100),
    ?assertEqual(2, arithmetic_tcp_client:add(1, 1)),
    ok = arithmetic_tcp_server:stop(),
    timer:sleep(100),
    ?assertEqual({error, no_socket}, arithmetic_tcp_client:add(1, 1)),
    ok = arithmetic_tcp_server:start(),
    timer:sleep(100),
    ?assertEqual(2, arithmetic_tcp_client:add(1, 1)).

reconnect_udp_subtest() ->
    ?assertEqual({error, no_socket}, arithmetic_udp_client:add(1, 1)),
    ok = arithmetic_udp_server:start(),
    timer:sleep(100),
    ?assertEqual(2, arithmetic_udp_client:add(1, 1)),
    ok = arithmetic_udp_server:stop(),
    timer:sleep(100),
    ?assertEqual({error, timeout}, arithmetic_udp_client:add(1, 1)),
    ok = arithmetic_udp_server:start(),
    timer:sleep(100),
    ?assertEqual(2, arithmetic_udp_client:add(1, 1)).

%% utils
assert_random_add(Client) ->
    A = rand(),
    B = rand(),
    ?assertEqual(A + B, Client:add(A, B)).

assert_random_multiply(Client) ->
    A = rand(),
    B = rand(),
    ?assertEqual(A * B, Client:multiply(A, B)).

cleanup() ->
    shackle_app:stop().

cleanup_ssl() ->
    arithmetic_ssl_client:stop(),
    arithmetic_ssl_server:stop(),
    cleanup().

cleanup_tcp() ->
    arithmetic_tcp_client:stop(),
    arithmetic_tcp_server:stop(),
    cleanup().

cleanup_udp() ->
    arithmetic_udp_client:stop(),
    arithmetic_udp_server:stop(),
    cleanup().

rand() ->
    shackle_utils:random(255).

receive_loop(0) ->
    [];
receive_loop(N) ->
    receive
        {response, X} ->
            [X | receive_loop(N - 1)]
    end.

setup() ->
    error_logger:tty(false),
    shackle_app:start().

setup_ssl(Options) ->
    setup(),
    arithmetic_ssl_server:start(),
    shackle_pool:start(?POOL_NAME, ?CLIENT_SSL, [
        {port, ?PORT},
        {protocol, shackle_ssl},
        {reconnect, true},
        {socket_options, [
            binary,
            {packet, raw}
        ]}
    ], Options).

setup_tcp(Options) ->
    setup(),
    arithmetic_tcp_server:start(),
    shackle_pool:start(?POOL_NAME, ?CLIENT_TCP, [
        {port, ?PORT},
        {protocol, shackle_tcp},
        {reconnect, true},
        {socket_options, [
            binary,
            {packet, raw}
        ]}
    ], Options).

setup_udp(Options) ->
    setup(),
    arithmetic_udp_server:start(),
    shackle_pool:start(?POOL_NAME, ?CLIENT_UDP, [
        {port, ?PORT},
        {protocol, shackle_udp},
        {reconnect, true},
        {socket_options, [
            binary
        ]}
    ], Options).
