-module(arithmetic_ssl_server).
-include("test.hrl").
-include_lib("eunit/include/eunit.hrl").

-ifdef(OTP_RELEASE).
-compile({nowarn_deprecated_function, [{ssl, ssl_accept, 3}]}).
-endif.

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
    case ssl:transport_accept(LSocket) of
        {ok, Socket} ->
            ok = ssl:ssl_accept(Socket),
            spawn(fun() -> loop(Socket, <<>>) end),
            accept(LSocket);
        {error, closed} ->
            ok
    end.

listen() ->
    Self = self(),
    spawn(fun () ->
        register(?MODULE, self()),
        Options = [
            binary,
            {backlog, 4096},
            {active, false},
            {reuseaddr, true},
            {certfile, <<"./test/cert.pem">>},
            {keyfile, <<"./test/key.pem">>}
        ],
        {ok, LSocket} = ssl:listen(?PORT, Options),
        Self ! LSocket,
        receive
            {kill, Pid} ->
                ssl:close(LSocket),
                unregister(?MODULE),
                Pid ! dead
        end
    end),
    receive
        LSocket ->
            LSocket
    end.

loop(Socket, Buffer) ->
    case ssl:recv(Socket, 0) of
        {ok, Requests} ->
            Requests2 = <<Buffer/binary, Requests/binary>>,
            {Replies, Buffer2} = arithmetic_protocol:parse_requests(Requests2),
            ok = ssl:send(Socket, Replies),
            loop(Socket, Buffer2);
        {error, closed} ->
            ok
    end.
