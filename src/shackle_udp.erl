-module(shackle_udp).
-include("shackle_internal.hrl").

-export([
    new/3,
    send/3,
    close/1
]).

-spec new(inet:ip_address(), inet:port_number(), [gen_udp:connect_option()]) ->
    {ok, inet:socket()} | {error, term()}.

new(_Ip, _Port, Options) ->
    gen_udp:open(0, Options).

-spec send(inet:socket(), iodata(), iodata()) -> ok | {error, term()}.

send(Socket, Header, Data) ->
    try erlang:port_command(Socket, [Header, Data]) of
        true ->
            ok;
        false ->
            {error, command_aborted}
    catch
        Error:Reason ->
            {error, {Error, Reason}}
    end.

-spec close(inet:socket()) -> ok.

close(Socket) ->
    gen_udp:close(Socket).
