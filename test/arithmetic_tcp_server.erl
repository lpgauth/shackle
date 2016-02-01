-module(arithmetic_tcp_server).
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
            {ok, LSocket} = listen(),
            put(?MODULE, LSocket),
            spawn(fun () -> accept(LSocket) end),
            ok;
        _LSocket ->
            {error, already_started}
    end.

-spec stop() ->
    ok | {error, not_started}.

stop() ->
    case get(?MODULE) of
        undefined ->
            {error, not_started};
        LSocket ->
            gen_tcp:close(LSocket),
            put(?MODULE, undefined),
            ok
    end.

%% private
accept(LSocket) ->
    case gen_tcp:accept(LSocket) of
        {ok, Socket} ->
            spawn(fun() -> loop(Socket, <<>>) end),
            accept(LSocket);
        {error, closed} ->
            ok
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
