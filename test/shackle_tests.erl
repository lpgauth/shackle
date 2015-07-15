-module(shackle_tests).
-include("test.hrl").

-compile(export_all).

-define(N, 1000).

%% runners
shackle_test_() ->
    {setup,
        fun () -> setup()end,
        fun (_) -> cleanup() end,
    {inparallel,[
        ?T(test_add),
        ?T(test_multiply)
    ]}}.

%% tests
test_add() ->
    [assert_random_add() || _ <- lists:seq(1, ?N)].

test_multiply() ->
    [assert_random_multiply() || _ <- lists:seq(1, ?N)].

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
    arithmetic_client:stop(),
    arithmetic_server:stop(),
    application:stop(shackle),
    error_logger:tty(true).

rand() ->
    random:uniform(255).

setup() ->
    random:seed(os:timestamp()),

    error_logger:tty(false),
    shackle_app:start(),
    application:start(shackle),

    arithmetic_server:start(),
    arithmetic_client:start(),

    error_logger:tty(true).

test(Test) ->
    {atom_to_list(Test), ?MODULE, Test}.
