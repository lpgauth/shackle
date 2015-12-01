-module(shackle_tcp).
-include("shackle_internal.hrl").

-export([
    new/3,
    send/3,
    close/1
]).

-spec new(inet:ip_address(), inet:port_number(), [gen_tcp:connect_option()]) ->
    {ok, inet:socket()} | {error, term()}.

new(Ip, Port, Options) ->
    gen_tcp:connect(Ip, Port, Options).

-spec send(inet:socket(), iodata(), iodata()) -> ok | {error, term()}.

send(Socket, _Header, Data) ->
    gen_tcp:send(Socket, Data).

-spec close(inet:socket()) -> ok.

close(Socket) ->
    gen_tcp:socket(Socket).
