-module(arithmetic_udp_server).
-include("test.hrl").

-export([
    start/0,
    stop/0
]).

%% public
-spec start() -> ok | {error, already_started}.

start() ->
    case whereis(arithmetic_udp_server) of
        undefined ->
            {ok, Socket} = open(),
            Pid = spawn(fun () -> loop(Socket, <<>>) end),
            register(arithmetic_udp_server, Pid),
            ok;
        _Pid ->
            {error, already_started}
    end.

-spec stop() -> ok | {error, not_started}.

stop() ->
    case whereis(arithmetic_udp_server) of
        undefined ->
            {error, not_started};
        Pid ->
            Pid ! {stop, self()},
            receive
                closed -> ok
            end
    end.

%% private
open() ->
    Options = [binary, {active, false}, {reuseaddr, true}],
    gen_udp:open(?PORT, Options).

loop(Socket, Buffer) ->
    receive_msg(Socket),
    case gen_udp:recv(Socket, 0, 500) of
        {ok, {{127, 0, 0, 1}, Port, Requests}} ->
            Requests2 = <<Buffer/binary, Requests/binary>>,
            {Replies, Buffer2} = arithmetic_protocol:parse_requests(Requests2),
            ok = gen_udp:send(Socket, "127.0.0.1", Port, Replies),
            loop(Socket, Buffer2);
        {error, timeout} ->
            loop(Socket, Buffer);
        {error, closed} ->
            ok
    end.

receive_msg(Socket) ->
    receive
        {stop, Pid} ->
            gen_udp:close(Socket),
            unregister(?MODULE),
            Pid ! closed
    after 0 ->
        ok
    end.
