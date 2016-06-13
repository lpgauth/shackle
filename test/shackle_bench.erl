-module(shackle_bench).
-include("test.hrl").

-export([
    run/0
]).

-define(N, 1000).
-define(P, 20).

%% public
-spec run() -> ok.

run() ->
    shackle_test_utils:preload_modules(),
    application:start(shackle),
    arithmetic_tcp_client:start(),

    Timing = timing:function(fun () ->
        arithmetic_tcp_client:add(10, 10)
    end, ?N, ?P),

    io:format("~p~n", [Timing]),

    arithmetic_tcp_client:stop(),
    application:stop(shackle),

    ok.
