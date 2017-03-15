-module(arithmetic_udp_server).
-include("test.hrl").

-export([
    start/0,
    stop/0
]).

%% public
-spec start() ->
    ok | {error, already_started}.

start() ->
    case whereis(?MODULE) of
        undefined ->
            spawn(fun () -> loop(open(), <<>>) end),
            ok;
        _Socket ->
            {error, already_started}
    end.

-spec stop() ->
    ok | {error, not_started}.

stop() ->
    case whereis(?MODULE) of
        undefined ->
            {error, not_started};
        Pid ->
            Pid ! {kill, self()},
            receive
                dead ->
                    ok
            end
    end.

%% private
loop(Socket, Buffer) ->
    case gen_udp:recv(Socket, 0) of
        {ok, {{127, 0, 0, 1}, Port, Requests}} ->
            Requests2 = <<Buffer/binary, Requests/binary>>,
            {Replies, Buffer2} = arithmetic_protocol:parse_requests(Requests2),
            ok = gen_udp:send(Socket, "127.0.0.1", Port, Replies),
            loop(Socket, Buffer2);
        {error, closed} ->
            ok
    end.

open() ->
    Self = self(),
    spawn(fun () ->
        register(?MODULE, self()),
        Options = [binary, {active, false}, {reuseaddr, true}],
        {ok, Socket} = gen_udp:open(?PORT, Options),
        Self ! Socket,
        receive
            {kill, Pid} ->
                gen_udp:close(Socket),
                unregister(?MODULE),
                Pid ! dead
        end
    end),
    receive
        Socket ->
            Socket
    end.
