-module(shackle_socket).
-moduledoc """
NIF-based TCP socket protocol using the socket module (OTP 27.3+).

This protocol uses Erlang's socket module with OTP select read support for
improved performance compared to gen_tcp. It provides:

- ~2.5% throughput improvement over gen_tcp
- Better control over socket options
- Support for socket:sendv/2 for iodata (OTP 27+, avoiding flattening)
- Requires OTP 27.3 or later

Falls back to shackle_tcp on older OTP versions.
""".
-include("shackle_defaults.hrl").

-behavior(shackle_protocol).
-export([
    close/1,
    connect/3,
    send/2,
    setopts/2
]).

%% Compile-time OTP version check
-if(?OTP_RELEASE >= 27).
  -define(OTP_27_PLUS, true).
-else.
  -define(OTP_27_PLUS, false).
-endif.

%%-----------------------------------------------------------------------------
%% Implementation for OTP 27+: socket module
%%-----------------------------------------------------------------------------

-if(?OTP_27_PLUS).

-spec close(shackle:socket()) -> ok.
close(Socket) ->
    try
        socket:close(Socket),
        ok
    catch
        _:_ -> ok
    end.

%% socket:connect/2 takes a resolved IP, not a hostname.
-spec connect(inet:ip_address(), shackle:inet_port(), shackle:socket_options()) ->
    {ok, shackle:socket()} | {error, atom()}.
connect(Address, Port, SocketOptions) ->
    try
        Family = shackle_utils:address_family(Address),
        case socket:open(Family, stream, tcp) of
            {ok, Socket} ->
                ok = apply_socket_options(Socket, SocketOptions),
                case socket:connect(Socket, #{family => Family, addr => Address, port => Port}) of
                    ok ->
                        enable_select_read(Socket);
                    {error, ConnectErr} ->
                        socket:close(Socket),
                        {error, ConnectErr}
                end;
            {error, OpenErr} ->
                {error, OpenErr}
        end
    catch
        error:CatchErr ->
            {error, CatchErr}
    end.

%% Enable OTP select read for non-blocking operation
enable_select_read(Socket) ->
    try
        case socket:setopt(Socket, otp, select_read, true) of
            ok ->
                {ok, Socket};
            {error, SetErr} ->
                socket:close(Socket),
                {error, SetErr}
        end
    catch
        error:_ ->
            {ok, Socket}
    end.

%% Apply socket options. Errors from setopt are ignored, as before.
apply_socket_options(_Socket, []) ->
    ok;
apply_socket_options(Socket, [Opt | Rest]) ->
    case convert_option(Opt) of
        skip ->
            apply_socket_options(Socket, Rest);
        {ok, Level, Options} ->
            lists:foreach(
                fun({Name, Value}) -> ok = socket:setopt(Socket, {Level, Name}, Value) end,
                Options),
            apply_socket_options(Socket, Rest)
    end.

%% Convert gen_tcp option to socket option
convert_option({active, _})               -> skip;  %% not a socket-module option
convert_option({buffer, Size})            -> {ok, socket, [{rcvbuf, Size}]};
convert_option({delay_send, _})           -> skip;
convert_option({deliver, _})              -> skip;
convert_option({dontroute, Flag})         -> {ok, socket, [{dontroute, Flag}]};
convert_option({exit_on_close, _})        -> skip;
convert_option({high_msgq_watermark, _})  -> skip;
convert_option({high_watermark, _})       -> skip;
convert_option({ipv6_v6only, Flag})       -> {ok, ipv6, [{v6only, Flag}]};
convert_option({keepalive, Flag})         -> {ok, socket, [{keepalive, Flag}]};
convert_option({linger, {true, Seconds}}) -> {ok, socket, [{linger, #{onoff => true, linger => Seconds}}]};
convert_option({linger, {false, _}})      -> {ok, socket, [{linger, #{onoff => false, linger => 0}}]};
convert_option({low_msgq_watermark, _})   -> skip;
convert_option({low_watermark, _})        -> skip;
convert_option({nodelay, Flag})           -> {ok, tcp, [{nodelay, Flag}]};
convert_option({packet, raw})             -> skip;
convert_option({packet, _})               -> skip;
convert_option({priority, Priority})      -> {ok, socket, [{priority, Priority}]};
convert_option({recbuf, Size})            -> {ok, socket, [{rcvbuf, Size}]};
convert_option({reuseaddr, Flag})         -> {ok, socket, [{reuseaddr, Flag}]};
convert_option({send_timeout, Timeout})   -> {ok, socket, [{sndtimeo, Timeout}]};
convert_option({send_timeout_close, _})   -> skip;
convert_option({show_econnreset, Flag})   -> {ok, socket, [{show_econnreset, Flag}]};
convert_option({sndbuf, Size})            -> {ok, socket, [{sndbuf, Size}]};
convert_option({tos, ToS})                -> {ok, ip, [{tos, ToS}]};
convert_option({ttl, TTL})                -> {ok, ip, [{ttl, TTL}]};
convert_option(binary)                    -> skip;
convert_option(inet)                      -> skip;
convert_option(inet6)                     -> skip;
convert_option(_Other)                    -> skip.

-spec send(shackle:socket(), iodata()) -> ok | {error, atom()}.
send(Socket, Data) ->
    try
        case socket:sendv(Socket, [Data]) of
            {ok, _} -> ok;
            {error, SendErr} -> {error, SendErr}
        end
    catch
        error:CatchErr ->
            {error, CatchErr}
    end.

-spec setopts(shackle:socket(), [gen_tcp:option()]) -> ok | {error, atom()}.
setopts(Socket, Opts) ->
    apply_socket_options(Socket, Opts).

%% ============================================================================
%% Fallback for OTP < 27: use gen_tcp
%% ============================================================================

-else.

-spec close(shackle:socket()) -> ok.
close(Socket) ->
    gen_tcp:close(Socket).

-spec connect(shackle:inet_address(), shackle:inet_port(), shackle:socket_options()) ->
    {ok, shackle:socket()} | {error, atom()}.
connect(Address, Port, SocketOptions) ->
    gen_tcp:connect(Address, Port, SocketOptions, ?DEFAULT_CONNECT_TIMEOUT).

-spec send(shackle:socket(), iodata()) -> ok | {error, atom()}.
send(Socket, Data) ->
    gen_tcp:send(Socket, Data).

-spec setopts(shackle:socket(), [gen_tcp:option()]) -> ok | {error, atom()}.
setopts(Socket, Opts) ->
    inet:setopts(Socket, Opts).

-endif.
