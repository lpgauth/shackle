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
    Filenames = filelib:wildcard("_build/default/lib/*/ebin/*.beam"),
    Rootnames = [filename:rootname(Filename, ".beam") || Filename <- Filenames],
    lists:foreach(fun code:load_abs/1, Rootnames),

    application:start(shackle),
    fprofx:start(),
    {ok, Tracer} = fprofx:profile(start),
    fprofx:trace([start, {procs, new}, {tracer, Tracer}]),

    arithmetic_tcp_client:start(),
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
    timer:sleep(500),
    ProcessCount = erlang:system_info(process_count),
    sleep(ProcessCount, erlang:system_info(process_count) - ?P).

sleep(X, X) ->
    ok;
sleep(_X, Y) ->
    timer:sleep(100),
    sleep(erlang:system_info(process_count), Y).
