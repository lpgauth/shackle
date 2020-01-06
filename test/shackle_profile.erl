-module(shackle_profile).
-include("test.hrl").

-export([
    run/0
]).

-define(CLIENT, arithmetic_tcp_client).
-define(N, 1000).
-define(P, 20).

%% public
-spec run() -> ok.

run() ->
    error_logger:tty(false),
    shackle_test_utils:preload_modules(),
    shackle_app:start(),

    fprofx:start(),
    {ok, Tracer} = fprofx:profile(start),
    fprofx:trace([start, {procs, new}, {tracer, Tracer}]),

    ?CLIENT:start(),
    timer:sleep(500),

    Self = self(),
    [spawn(fun () ->
        [20 = ?CLIENT:add(10, 10) || _ <- lists:seq(1, ?N)],
        Self ! exit
    end) || _ <- lists:seq(1, ?P)],
    wait(),

    fprofx:trace(stop),
    fprofx:analyse([totals, {dest, ""}]),
    fprofx:stop(),

    ?CLIENT:stop(),
    application:stop(shackle),

    ok.

%% private
wait() ->
    wait(?P).

wait(0) ->
    ok;
wait(X) ->
    receive
        exit ->
            wait(X - 1)
    end.
