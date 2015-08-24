-module(shackle_tests).
-include("test.hrl").

%% runners
shackle_backlog_test_() ->
    {setup,
        fun () ->
            setup(),
            shackle_pool:start(?POOL_NAME, ?CLIENT, [
                {backlog_size, 1},
                {pool_size, 1}
            ])
        end,
        fun (_) -> cleanup() end,
    [fun backlog_full_subtest/0]}.

shackle_random_test_() ->
    {setup,
        fun () ->
            setup(),
            arithmetic_client:start()
        end,
        fun (_) -> cleanup() end,
    {inparallel, [
        fun add_subtest/0,
        fun multiply_subtest/0
    ]}}.

shackle_reconnect_test_() ->
    {setup,
        fun () ->
            setup(),
            arithmetic_server:stop(),
            shackle_pool:start(?POOL_NAME, ?CLIENT, [
                {pool_size, 1}
            ])
        end,
        fun (_) -> cleanup() end,
    [fun reconnect_subtest/0]}.

shackle_round_robin_test() ->
    {setup,
        fun () ->
            setup(),
            shackle_pool:start(?POOL_NAME, ?CLIENT, [
                {pool_strategy, round_robin}
            ])
        end,
        fun (_) -> cleanup() end,
    {inparallel, [
        fun add_subtest/0,
        fun multiply_subtest/0
    ]}}.

%% tests
add_subtest() ->
    [assert_random_add() || _ <- lists:seq(1, ?N)].

backlog_full_subtest() ->
    Pid = self(),
    [spawn(fun () ->
        X = arithmetic_client:add(1, 1),
        Pid ! {response, X}
    end) || _ <- lists:seq(1, 20)],

    ?assert(lists:any(fun
        ({error, backlog_full}) -> true;
        (_) -> false
    end, receive_loop(20))).

multiply_subtest() ->
    [assert_random_multiply() || _ <- lists:seq(1, ?N)].

reconnect_subtest() ->
    ?assertEqual({error, no_socket}, arithmetic_client:add(1, 1)),
    arithmetic_server:start(),
    timer:sleep(3000),
    ?assertEqual(2, arithmetic_client:add(1, 1)).

%% utils
assert_random_add() ->
    A = rand(),
    B = rand(),
    ?assertEqual(A + B, arithmetic_client:add(A, B)).

assert_random_multiply() ->
    A = rand(),
    B = rand(),
    ?assertEqual(A * B, arithmetic_client:multiply(A, B)).

cleanup() ->
    arithmetic_server:stop(),
    arithmetic_client:stop(),
    shackle_app:stop().

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
    arithmetic_server:start(),
    shackle_app:start().
