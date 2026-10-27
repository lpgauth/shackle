-module(shackle_observe_prometheus_metrics_test).

-include_lib("eunit/include/eunit.hrl").

%%%
%%% Simple Tests - Each one verifies one metric is called
%%%

%% Test 1: shackle_request_total is not incremented on call stop
%% (it is counted per socket send in shackle_server.erl instead)
shackle_request_total_test() ->
    Cleanup = init_prometheus(),

    Client = <<"test_service">>,
    Pool = <<"pool1">>,
    Metadata = #{pool => Pool, client => Client},

    ok = shackle_observe:event([call, stop], #{}, Metadata),

    History = meck:history(prometheus_counter),
    Calls = [C || {_, {prometheus_counter, inc, [shackle_request_total, _, _]}, _} = C <- History],

    shackle_test_utils:cleanup_mocks(Cleanup),
    ?assertEqual([], Calls).

%% Test 2: shackle_cast_total
shackle_cast_total_test() ->
    Cleanup = init_prometheus(),

    Client = <<"test_service">>,
    Pool = <<"pool1">>,
    Metadata = #{pool => Pool, client => Client},

    ok = shackle_observe:event([cast, stop], #{}, Metadata),

    History = meck:history(prometheus_counter),
    Calls = [C || {_, {prometheus_counter, inc, [shackle_cast_total, [_Client, _Pool], 1]}, _} = C <- History],

    shackle_test_utils:cleanup_mocks(Cleanup),
    ?assertEqual(1, length(Calls)).

%% Test 6: shackle_error_total (exception)
shackle_error_total_exception_test() ->
    Cleanup = init_prometheus(),

    Client = <<"test_service">>,
    Pool = <<"pool1">>,
    Metadata = #{pool => Pool, client => Client},

    ok = shackle_observe:event([call, exception], #{}, Metadata),

    History = meck:history(prometheus_counter),
    Calls = [C ||
        {_, {prometheus_counter, inc,
             [shackle_error_total, [_Client, _Pool, <<"exception">>], 1]}, _} = C
        <- History],

    shackle_test_utils:cleanup_mocks(Cleanup),
    ?assertEqual(1, length(Calls)).

init_prometheus() ->
    OldVal = application:get_env(shackle, observability),
    persistent_term:erase(shackle_observe),
    application:set_env(shackle, observability, prometheus),
    Cleanup = shackle_test_utils:with_prometheus([{shackle, observability, OldVal}]),
    ok = shackle_observe_prometheus:start(),
    [fun() -> persistent_term:erase(shackle_observe) end | Cleanup].
