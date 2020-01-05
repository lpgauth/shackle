-module(shackle_udp).
-include("shackle_internal.hrl").

-behavior(shackle_protocol).
-export([
    close/1,
    connect/3,
    send/2,
    setopts/2
]).

%% callbacks
-spec close(socket()) ->
    ok.

close(Socket) ->
    gen_udp:close(Socket).

-spec connect(inet_address(), inet_port(), socket_options()) ->
    {ok, socket()} | {error, atom()}.

connect(Address, Port, SocketOptions) ->
    case gen_udp:open(0, SocketOptions) of
        {ok, Socket} ->
            ok = gen_udp:connect(Socket, Address, Port),
            {ok, Socket};
        {error, Reason} ->
            {error, Reason}
    end.

-spec send(socket(), iodata()) ->
    ok | {error, atom()}.

send(Socket, Data) ->
    gen_udp:send(Socket, Data).

-spec setopts(socket(), [gen_udp:option()]) ->
    ok |
    {error, atom()}.

setopts(Socket, Opts) ->
    inet:setopts(Socket, Opts).
