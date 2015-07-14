-module(arithmetic_server).
-include("test.hrl").

-export([
    start/0,
    stop/0
]).

%% public
start() ->
    {ok, LSocket} = gen_tcp:listen(?PORT, ?TCP_OPTIONS),
    Pid = spawn(fun () -> accept(LSocket) end),
    register(arithmetic_server, Pid).

stop() ->
    case whereis(arithmetic_server) of
        undefined -> ok;
        Pid -> exit(Pid, kill)
    end.

%% private
accept(LSocket) ->
    case gen_tcp:accept(LSocket) of
        {error, closed} ->
            io:format("closed~n", []);
        {ok, Socket} ->
            spawn(fun() -> loop(Socket) end),
            accept(LSocket)
    end.

loop(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, <<ReqId:8/integer, 1, A:8/integer, B:8/integer>>} ->
            io:format("A: ~p, B: ~p~n", [A, B]),
            Reply = <<ReqId:8/integer, (A + B):16/integer>>,
            gen_tcp:send(Socket, Reply),
            loop(Socket);
        {ok, <<ReqId:8/integer, 2, A:8/integer, B:8/integer>>} ->
            Reply = <<ReqId:8/integer, (A * B):16/integer>>,
            gen_tcp:send(Socket, Reply),
            loop(Socket);
        {ok, X} ->
            ?debugFmt("~p~n", [X]),
            loop(Socket);
        {error, closed} ->
            ok
    end.
