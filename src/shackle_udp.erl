-module(shackle_udp).

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
    gen_udp:close(Socket).

-spec connect(shackle:inet_address(), shackle:inet_port(), shackle:socket_options()) ->
    {ok, shackle:socket()} | {error, atom()}.

connect(Address, Port, SocketOptions) ->
    case gen_udp:open(0, SocketOptions) of
        {ok, Socket} ->
            ok = gen_udp:connect(Socket, Address, Port),
            {ok, Socket};
        {error, Reason} ->
            {error, Reason}
    end.

-spec send(shackle:socket(), iodata()) ->
    ok | {error, atom()}.

send(Socket, Data) ->
    gen_udp:send(Socket, Data).

-spec setopts(shackle:socket(), [gen_udp:option()]) ->
    ok |
    {error, atom()}.

setopts(Socket, Opts) ->
    inet:setopts(Socket, Opts).
