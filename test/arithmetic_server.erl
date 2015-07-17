-module(arithmetic_server).
-include("../test/test.hrl").

-export([
    start/0,
    stop/0
]).

%% public
-spec start() -> ok | {error, already_started}.

start() ->
    case whereis(arithmetic_server) of
        undefined ->
            {ok, LSocket} = gen_tcp:listen(?PORT, [binary, {active, false}, {reuseaddr, true}]),
            Pid = spawn(fun () -> accept(LSocket) end),
            register(arithmetic_server, Pid),
            ok;
        _Pid ->
            {error, already_started}
    end.

-spec stop() -> ok | {error, not_started}.

stop() ->
    case whereis(arithmetic_server) of
        undefined ->
            {error, not_started};
        Pid ->
            Pid ! stop,
            ok
    end.

%% private
accept(LSocket) ->
    case gen_tcp:accept(LSocket, 0) of
        {ok, Socket} ->
            spawn(fun() -> loop(Socket, <<>>) end),
            accept(LSocket);
        {error, closed} ->
            ok;
        {error, timeout} ->
            ok
    end,
    receive
        stop ->
            gen_tcp:close(LSocket),
            unregister(?MODULE),
            ok
    after 0 ->
        accept(LSocket)
    end.

loop(Socket, Buffer) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Requests} ->
            Requests2 = <<Buffer/binary, Requests/binary>>,
            {Replies, Buffer2} = parse_requests(Requests2, []),
            gen_tcp:send(Socket, Replies),
            loop(Socket, Buffer2);
        {error, closed} ->
            ok
    end.

parse_requests(<<ReqId:8/integer, 1, A:8/integer, B:8/integer, Rest/binary>>, Acc) ->
    parse_requests(Rest, [<<ReqId:8/integer, (A + B):16/integer>> | Acc]);
parse_requests(<<ReqId:8/integer, 2, A:8/integer, B:8/integer, Rest/binary>>, Acc) ->
    parse_requests(Rest, [<<ReqId:8/integer, (A * B):16/integer>> | Acc]);
parse_requests(Buffer, Acc) ->
    {Acc, Buffer}.
