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

shackle_random_tcp_test_() ->
    {setup,
        fun () -> setup_tcp([
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
            {pool_strategy, random}
        ]) end,
        fun (_) -> cleanup_udp() end,
    {inparallel, [
        fun add_udp_subtest/0,
        fun multiply_udp_subtest/0
    ]}}.

shackle_reconnect_test_() ->
    {setup,
        fun () ->
            setup(),
            shackle_pool:start(?POOL_NAME, ?CLIENT_TCP, [
                {port, ?PORT},
                {reconnect, true},
                {socket_options, [
                    binary,
                    {packet, raw}
                ]}], [{pool_size, 1}])
        end,
        fun (_) -> cleanup_tcp() end,
    [fun reconnect_subtest/0]}.

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

multiply_tcp_subtest() ->
    [assert_random_multiply(?CLIENT_TCP) || _ <- lists:seq(1, ?N)].

multiply_udp_subtest() ->
    [assert_random_multiply(?CLIENT_UDP) || _ <- lists:seq(1, ?N)].

reconnect_subtest() ->
    ?assertEqual({error, no_socket}, arithmetic_tcp_client:add(1, 1)),
    arithmetic_tcp_server:start(),
    timer:sleep(3000),
    ?assertEqual(2, arithmetic_tcp_client:add(1, 1)).

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

cleanup_tcp() ->
    arithmetic_tcp_client:stop(),
    arithmetic_tcp_server:stop(),
    cleanup().

cleanup_udp() ->
    arithmetic_udp_client:stop(),
    arithmetic_udp_server:stop(),
    cleanup().

rand() ->
    random:uniform(255).

receive_loop(0) -> [];
receive_loop(N) ->
    receive
        {response, X} ->
            [X | receive_loop(N - 1)]
    end.

setup() ->
    random:seed(os:timestamp()),
    error_logger:tty(false),
    shackle_app:start().

setup_tcp(Options) ->
    setup(),
    arithmetic_tcp_server:start(),
    shackle_pool:start(?POOL_NAME, ?CLIENT_TCP, [
        {port, ?PORT},
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
