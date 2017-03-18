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
    case whereis(?MODULE) of
        undefined ->
            spawn(fun () -> accept(listen()) end),
            ok;
        _Pid ->
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
accept(LSocket) ->
    case gen_tcp:accept(LSocket) of
        {ok, Socket} ->
            spawn(fun() -> loop(Socket, <<>>) end),
            accept(LSocket);
        {error, closed} ->
            ok
    end.

listen() ->
    Self = self(),
    spawn(fun () ->
        register(?MODULE, self()),
        Options = [binary, {backlog, 4096}, {active, false}, {reuseaddr, true}],
        {ok, LSocket} = gen_tcp:listen(?PORT, Options),
        Self ! LSocket,
        receive
            {kill, Pid} ->
                gen_tcp:close(LSocket),
                unregister(?MODULE),
                Pid ! dead
        end
    end),
    receive
        LSocket ->
            LSocket
    end.

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
