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
    case get(?MODULE) of
        undefined ->
            {ok, Socket} = open(),
            put(?MODULE, Socket),
            spawn(fun () -> loop(Socket, <<>>) end),
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
        Socket ->
            gen_udp:close(Socket),
            put(?MODULE, undefined),
            ok
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
    Options = [binary, {active, false}, {reuseaddr, true}],
    gen_udp:open(?PORT, Options).
