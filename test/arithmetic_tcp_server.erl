-module(arithmetic_tcp_server).
-include("test.hrl").

-export([
    start/0,
    stop/0
]).

%% public
-spec start() -> ok | {error, already_started}.

start() ->
    case whereis(arithmetic_tcp_server) of
        undefined ->
            {ok, LSocket} = listen(),
            Pid = spawn(fun () -> accept(LSocket) end),
            register(arithmetic_tcp_server, Pid),
            ok;
        _Pid ->
            {error, already_started}
    end.

-spec stop() -> ok | {error, not_started}.

stop() ->
    case whereis(arithmetic_tcp_server) of
        undefined ->
            {error, not_started};
        Pid ->
            Pid ! {stop, self()},
            receive
                closed -> ok
            end
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
            receive_msg(LSocket)
    end.

listen() ->
    Options = [binary, {backlog, 4096}, {active, false}, {reuseaddr, true}],
    gen_tcp:listen(?PORT, Options).

loop(Socket, Buffer) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Requests} ->
            Requests2 = <<Buffer/binary, Requests/binary>>,
            {Replies, Buffer2} = arithmetic_protocol:parse_requests(Requests2),
            ok = gen_tcp:send(Socket, Replies),
            loop(Socket, Buffer2);
        {error, closed} ->
            ok
    end.

receive_msg(LSocket) ->
    receive
        {stop, Pid} ->
            gen_tcp:close(LSocket),
            unregister(?MODULE),
            Pid ! closed
    after 0 ->
        accept(LSocket)
    end.
