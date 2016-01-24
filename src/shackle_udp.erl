-module(shackle_udp).
-include("shackle_internal.hrl").

-export([
    close/1,
    header/2,
    new/3,
    send/3
]).

%% public
-spec close(inet:socket()) ->
    ok.

close(Socket) ->
    gen_udp:close(Socket).

-spec header(inet:ip_address(), inet:port_number()) -> iodata().

header({A, B, C, D}, Port) ->
    [[((Port) bsr 8) band 16#ff, (Port) band 16#ff],
        [A band 16#ff, B band 16#ff, C band 16#ff, D band 16#ff]].

-spec new(inet:ip_address(), inet:port_number(), [gen_udp:option()]) ->
    {ok, inet:socket()} | {error, term()}.

new(_Ip, _Port, Options) ->
    gen_udp:open(0, Options).

-spec send(inet:socket(), iodata(), iodata()) ->
    ok | {error, term()}.

send(Socket, Header, Data) ->
    try
        true = erlang:port_command(Socket, [Header, Data]),
        ok
    catch
        Error:Reason ->
            {error, {Error, Reason}}
    end.
