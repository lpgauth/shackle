-module(shackle_profile).
-include("test.hrl").

-export([
    fprofx/0
]).

-define(N, 1000).
-define(P, 20).

%% public
-spec fprofx() -> ok.

fprofx() ->
    application:start(shackle),
    arithmetic_tcp_client:start(),
    [20 = arithmetic_tcp_client:add(10, 10) || _ <- lists:seq(1, 10)],

    fprofx:start(),
    {ok, Tracer} = fprofx:profile(start),
    fprofx:trace([start, {procs, all}, {tracer, Tracer}]),

    [spawn(fun () ->
        [20 = arithmetic_tcp_client:add(10, 10) || _ <- lists:seq(1, ?N)]
    end) || _ <- lists:seq(1, ?P)],

    sleep(),

    fprofx:trace(stop),
    fprofx:analyse([totals, {dest, ""}]),
    fprofx:stop(),

    ok.

%% private
sleep() ->
    ProcessCount = erlang:system_info(process_count),
    sleep(ProcessCount, erlang:system_info(process_count) - ?P).

sleep(X, X) ->
    ok;
sleep(_X, Y) ->
    timer:sleep(100),
    sleep(erlang:system_info(process_count), Y).
