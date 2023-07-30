-module(shackle_protocol).

%% callbacks
-callback close(Socket :: shackle:socket()) ->
    ok.

-callback connect(Address :: shackle:inet_address(), Port :: shackle:inet_port(),
    SocketOptions :: shackle:socket_options()) ->

    {ok, Socket :: shackle:socket()} |
    {error, atom()}.

-callback send(Socket :: shackle:socket(), iodata()) ->
    ok |
    {error, atom()}.

-callback setopts(Socket :: shackle:socket(), [gen_tcp:option() | gen_udp:option()]) ->
    ok |
    {error, atom()}.
