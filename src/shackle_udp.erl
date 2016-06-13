-module(shackle_udp).
-include("shackle_internal.hrl").

-compile(inline).
-compile({inline_size, 512}).

-export([
    close/1,
    header/2,
    new/3,
    send/3
]).

-define(INET_AF_INET, 1).
-define(INT16(X), [((X) bsr 8) band 16#ff, (X) band 16#ff]).

%% public
-spec close(inet:socket()) ->
    ok.

close(Socket) ->
    gen_udp:close(Socket).

-spec header(inet:ip_address(), inet:port_number()) -> iodata().

-ifdef(UDP_HEADER).

header(IP, Port) ->
    [?INET_AF_INET, ?INT16(Port) | ip4_to_bytes(IP)].

-else.

header(IP, Port) ->
    [?INT16(Port) | ip4_to_bytes(IP)].

-endif.

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

%% private
ip4_to_bytes({A, B, C, D}) ->
    [A band 16#ff, B band 16#ff, C band 16#ff, D band 16#ff].
