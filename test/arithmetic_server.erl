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
    {ok, Socket} = gen_tcp:accept(LSocket),
    spawn(fun() -> loop(Socket) end),
    accept(LSocket).

loop(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, <<ReqId:8/integer, 1, A:8/integer, B:8/integer>>} ->
            Reply = <<ReqId:8/integer, (integer_to_binary(A + B))/binary>>,
            gen_tcp:send(Socket, Reply),
            loop(Socket);
        {ok, <<ReqId:8/integer, 2, A:8/integer, B:8/integer>>} ->
            Reply = <<ReqId:8/integer, (integer_to_binary(A * B))/binary>>,
            gen_tcp:send(Socket, Reply),
            loop(Socket);
        {error, closed} ->
            ok
    end.
