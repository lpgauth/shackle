-module(shackle_tests).
-include("test.hrl").

-compile(export_all).

%% runners
shackle_test_() ->
    {setup,
        fun () -> setup()end,
        fun (_) -> cleanup() end,
    % {inparallel,
    [
        ?T(test_add),
        ?T(test_multiply)
    ]
    % }
    }.

%% tests
test_add() ->
    assert_add(340, 193, 147),
    assert_add(115, 110, 5),
    assert_add(429, 180, 249),
    assert_add(297, 45, 252),
    assert_add(180, 91, 89).

test_multiply() ->
    assert_multiply(19197, 79, 243),
    assert_multiply(18765, 135, 139),
    assert_multiply(13380, 223, 60),
    assert_multiply(17400, 232, 75),
    assert_multiply(29820, 140, 213).

%% utils
assert_add(Expected, A, B) ->
    ?assertEqual(Expected, arithmetic_client:add(A, B)).

assert_multiply(Expected, A, B) ->
    ?assertEqual(Expected, arithmetic_client:multiply(A, B)).

cleanup() ->
    error_logger:tty(false),
    arithmetic_client:stop(),
    arithmetic_server:stop(),
    application:stop(shackle),
    error_logger:tty(true).

setup() ->
    % error_logger:tty(false),
    shackle_app:start(),
    application:start(shackle),

    arithmetic_server:start(),
    arithmetic_client:start(),

    error_logger:tty(true).

test(Test) ->
    {atom_to_list(Test), ?MODULE, Test}.
