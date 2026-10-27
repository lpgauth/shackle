-module(shackle_ssl_socket).
-moduledoc """
NIF-based TLS socket protocol using the socket module (OTP 28+).

This protocol combines the socket module with ssl:connect for TLS support,
providing improved performance compared to ssl:connect with gen_tcp.

Features:
- Uses socket module for better control
- TLS negotiation via ssl:connect/3
- Combines raw socket + inet options + ssl
- Requires OTP 28 or later

Falls back to shackle_ssl on older OTP versions.
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
-if(?OTP_RELEASE >= 28).
  -define(OTP_28_PLUS, true).
-else.
  -define(OTP_28_PLUS, false).
-endif.

%%-----------------------------------------------------------------------------
%% Implementation for OTP 28+: socket module + ssl
%%-----------------------------------------------------------------------------

-if(?OTP_28_PLUS).

-spec close(shackle:socket()) -> ok.
close(Socket) ->
    try
        ssl:close(Socket),
        ok
    catch
        _:_ -> ok
    end.

-spec connect(shackle:inet_address(), shackle:inet_port(), shackle:socket_options()) ->
    {ok, shackle:socket()} | {error, atom()}.
connect(Address, Port, SocketOptions) ->
    try
        Family = shackle_utils:address_family(Address),
        case socket:open(Family, stream, tcp) of
            {ok, Socket} ->
                case apply_socket_options(Socket, SocketOptions) of
                    ok ->
                        TLSOptions = extract_tls_options(SocketOptions),
                        case ssl:connect(Address, Port, TLSOptions, ?DEFAULT_CONNECT_TIMEOUT) of
                            {ok, SSLSocket} ->
                                socket:close(Socket),
                                {ok, SSLSocket};
                            {error, ConnectErr} ->
                                socket:close(Socket),
                                {error, ConnectErr}
                        end;
                    {error, _} = SetoptErr ->
                        socket:close(Socket),
                        SetoptErr
                end;
            {error, OpenErr} ->
                {error, OpenErr}
        end
    catch
        error:CatchErr ->
            {error, CatchErr}
    end.

%% Extract TLS-specific options for ssl:connect
extract_tls_options(Options) ->
    extract_tls_options(Options, []).

extract_tls_options([], Acc) ->
    Acc;
extract_tls_options([Opt | Rest], Acc) ->
    case is_tls_option(Opt) of
        true ->
            extract_tls_options(Rest, [Opt | Acc]);
        false ->
            extract_tls_options(Rest, Acc)
    end.

%% Check if an option is relevant for TLS
is_tls_option({verify, _}) -> true;
is_tls_option({fail_if_no_peer_cert, _}) -> true;
is_tls_option({cacerts, _}) -> true;
is_tls_option({cafile, _}) -> true;
is_tls_option({capath, _}) -> true;
is_tls_option({cert, _}) -> true;
is_tls_option({certfile, _}) -> true;
is_tls_option({key, _}) -> true;
is_tls_option({keyfile, _}) -> true;
is_tls_option({password, _}) -> true;
is_tls_option({ciphers, _}) -> true;
is_tls_option({ciphersuites, _}) -> true;
is_tls_option({versions, _}) -> true;
is_tls_option({eccs, _}) -> true;
is_tls_option({signature_algs, _}) -> true;
is_tls_option({sni_hosts, _}) -> true;
is_tls_option({reuse_sessions, _}) -> true;
is_tls_option({reuse_session, _}) -> true;
is_tls_option({server_name_indication, _}) -> true;
is_tls_option({alpn_advertised_protocols, _}) -> true;
is_tls_option({alpn_preferred_protocols, _}) -> true;
is_tls_option({user_lookup_fun, _}) -> true;
is_tls_option({srp_identity, _}) -> true;
is_tls_option({psk_identity, _}) -> true;
is_tls_option({customize_hostname_check, _}) -> true;
is_tls_option({log_level, _}) -> true;
is_tls_option({ssl_imp, _}) -> true;
is_tls_option({honor_cipher_order, _}) -> true;
is_tls_option({padding_check, _}) -> true;
is_tls_option({client_preferred_next_protocols, _}) -> true;
is_tls_option({cert_reqs, _}) -> true;
is_tls_option({depth, _}) -> true;
is_tls_option({partial_chain, _}) -> true;
is_tls_option({fallback, _}) -> true;
is_tls_option(_) -> false.

%% Apply socket options (for TCP layer before TLS). A setopt error is returned to
%% the caller, which closes the socket and fails the connect.
apply_socket_options(_Socket, []) ->
    ok;
apply_socket_options(Socket, [Opt | Rest]) ->
    case convert_option(Opt) of
        skip ->
            apply_socket_options(Socket, Rest);
        {ok, Level, Options} ->
            case apply_setopts(Socket, Level, Options) of
                ok -> apply_socket_options(Socket, Rest);
                {error, _} = Error -> Error
            end
    end.

apply_setopts(_Socket, _Level, []) ->
    ok;
apply_setopts(Socket, Level, [{Name, Value} | T]) ->
    case socket:setopt(Socket, {Level, Name}, Value) of
        ok -> apply_setopts(Socket, Level, T);
        {error, _} = Error -> Error
    end.

%% Convert gen_tcp option to socket option
convert_option({active, _}) -> skip;  %% not a socket-module option
convert_option({buffer, Size}) -> {ok, socket, [{rcvbuf, Size}]};
convert_option({delay_send, _}) -> skip;
convert_option({deliver, _}) -> skip;
convert_option({dontroute, Flag}) -> {ok, socket, [{dontroute, Flag}]};
convert_option({exit_on_close, _}) -> skip;
convert_option({high_msgq_watermark, _}) -> skip;
convert_option({high_watermark, _}) -> skip;
convert_option({ipv6_v6only, Flag}) -> {ok, ipv6, [{v6only, Flag}]};
convert_option({keepalive, Flag}) -> {ok, socket, [{keepalive, Flag}]};
convert_option({linger, {true, Seconds}}) -> {ok, socket, [{linger, #{onoff => true, linger => Seconds}}]};
convert_option({linger, {false, _}}) -> {ok, socket, [{linger, #{onoff => false, linger => 0}}]};
convert_option({low_msgq_watermark, _}) -> skip;
convert_option({low_watermark, _}) -> skip;
convert_option({nodelay, Flag}) -> {ok, tcp, [{nodelay, Flag}]};
convert_option({packet, raw}) -> skip;
convert_option({packet, _}) -> skip;
convert_option({priority, Priority}) -> {ok, socket, [{priority, Priority}]};
convert_option({recbuf, Size}) -> {ok, socket, [{rcvbuf, Size}]};
convert_option({reuseaddr, Flag}) -> {ok, socket, [{reuseaddr, Flag}]};
convert_option({send_timeout, Timeout}) -> {ok, socket, [{sndtimeo, Timeout}]};
convert_option({send_timeout_close, _}) -> skip;
convert_option({show_econnreset, Flag}) -> {ok, socket, [{show_econnreset, Flag}]};
convert_option({sndbuf, Size}) -> {ok, socket, [{sndbuf, Size}]};
convert_option({tos, ToS}) -> {ok, ip, [{tos, ToS}]};
convert_option({ttl, TTL}) -> {ok, ip, [{ttl, TTL}]};
convert_option(binary) -> skip;
convert_option(inet) -> skip;
convert_option(inet6) -> skip;
convert_option(_Other) -> skip.

-spec send(shackle:socket(), iodata()) -> ok | {error, atom()}.
send(Socket, Data) ->
    try
        ssl:send(Socket, Data)
    catch
        error:SendErr ->
            {error, SendErr}
    end.

-spec setopts(shackle:socket(), [gen_tcp:option()]) -> ok | {error, atom()}.
setopts(Socket, Opts) ->
    try
        ssl:setopts(Socket, Opts)
    catch
        error:SetErr ->
            {error, SetErr}
    end.

%% ============================================================================
%% Fallback for OTP < 28: use shackle_ssl
%% ============================================================================

-else.

-spec close(shackle:socket()) -> ok.
close(Socket) ->
    ssl:close(Socket),
    ok.

-spec connect(shackle:inet_address(), shackle:inet_port(), shackle:socket_options()) ->
    {ok, shackle:socket()} | {error, atom()}.
connect(Address, Port, SocketOptions) ->
    ssl:connect(Address, Port, SocketOptions, ?DEFAULT_CONNECT_TIMEOUT).

-spec send(shackle:socket(), iodata()) -> ok | {error, atom()}.
send(Socket, Data) ->
    ssl:send(Socket, Data).

-spec setopts(shackle:socket(), [gen_tcp:option()]) -> ok | {error, atom()}.
setopts(Socket, Opts) ->
    ssl:setopts(Socket, Opts).

-endif.
