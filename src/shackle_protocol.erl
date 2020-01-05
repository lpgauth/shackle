-module(shackle_protocol).
-include("shackle_internal.hrl").

%% callbacks
-callback close(Socket :: socket()) ->
    ok.

-callback connect(Address :: inet_address(), Port :: inet_port(),
    SocketOptions :: socket_options()) ->

    {ok, Socket :: socket()} |
    {error, atom()}.

-callback send(Socket :: socket(), iodata()) ->
    ok |
    {error, atom()}.

-callback setopts(Socket :: socket(), [gen_tcp:option() | gen_udp:option()]) ->
    ok |
    {error, atom()}.
