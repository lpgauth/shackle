-module(shackle_tests).
-include("test.hrl").

-compile(export_all).

-define(N, 1000).

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
    [?T(test_backlog_full)]}.

shackle_random_test_() ->
    {setup,
        fun () ->
            setup(),
            shackle_pool:start(?POOL_NAME, ?CLIENT)
        end,
        fun (_) -> cleanup() end,
    {inparallel,[
        ?T(test_add),
        ?T(test_multiply)
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
    [?T(test_reconnect)]}.

shackle_round_robin_test_() ->
    {setup,
        fun () ->
            setup(),
            shackle_pool:start(?POOL_NAME, ?CLIENT, [
                {pool_size, 2},
                {pool_strategy, round_robin}
            ])
        end,
        fun (_) -> cleanup() end,
    {inparallel,[
        ?T(test_add),
        ?T(test_multiply)
    ]}}.

%% tests
test_add() ->
    [assert_random_add() || _ <- lists:seq(1, ?N)].

test_backlog_full() ->
    Pid = self(),
    [spawn(fun () ->
        X = arithmetic_client:add(1, 1),
        Pid ! {response, X}
    end) || _ <- lists:seq(1, 20)],

    ?assert(lists:any(fun
        ({error, backlog_full}) -> true;
        (_) -> false
    end, receive_loop(20))).

test_multiply() ->
    [assert_random_multiply() || _ <- lists:seq(1, ?N)].

test_reconnect() ->
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
    error_logger:tty(false),
    arithmetic_server:stop(),
    arithmetic_client:stop(),
    shackle_app:stop(),
    error_logger:tty(true).

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
    shackle_app:start(),
    error_logger:tty(true).

test(Test) ->
    {atom_to_list(Test), ?MODULE, Test}.
