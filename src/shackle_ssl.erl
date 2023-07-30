-module(shackle_ssl).
-include("shackle_internal.hrl").

-behavior(shackle_protocol).
-export([
    close/1,
    connect/3,
    send/2,
    setopts/2
]).

%% callbacks
-spec close(shackle:socket()) ->
    ok.

close(Socket) ->
    ssl:close(Socket),
    ok.

-spec connect(shackle:inet_address(), shackle:inet_port(), shackle:socket_options()) ->
    {ok, shackle:socket()} | {error, atom()}.

connect(Address, Port, SocketOptions) ->
    ssl:connect(Address, Port, SocketOptions, ?DEFAULT_CONNECT_TIMEOUT).

-spec send(shackle:socket(), iodata()) ->
    ok | {error, atom()}.

send(Socket, Data) ->
    ssl:send(Socket, Data).

-spec setopts(shackle:socket(), [gen_tcp:option()]) ->
    ok |
    {error, atom()}.

setopts(Socket, Opts) ->
    ssl:setopts(Socket, Opts).
