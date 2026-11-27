-module(shackle_socket_ktls).
-include("shackle_internal.hrl").

%% shackle_tcp_ktls over the socket NIF: ssl does the TLS 1.3
%% handshake on a socket:socket(), hands the record keys to the kernel
%% and exits, and the connection then runs like shackle_socket. Stock
%% OTP only supports the kTLS handover on gen_tcp, so this needs an
%% ssl with the socket handover patch (lpgauth/otp ktls-socket), Linux
%% and the `tls` kernel module.
-ignore_xref([{ssl_gen_statem, ktls_handover, 1},
              {inet_tls_dist, set_ktls, 1}]).
-dialyzer({nowarn_function, [connect/3, error_reason/1, handover/1, handshake/3,
                            set_ktls/2]}).

-behavior(shackle_protocol).
-export([
    close/1,
    connect/3,
    send/2,
    setopts/2
]).

-define(SOL_TLS, 282).
-define(TLS_RX_EXPECT_NO_PAD, 4).
-define(KTLS_CIPHER, #{cipher => aes_256_gcm, key_exchange => any,
                       mac => aead, prf => sha384}).

%% callbacks
-spec close(shackle:socket()) ->
    ok.

close(Socket) ->
    shackle_socket:close(Socket).

-spec connect(shackle:inet_address(), shackle:inet_port(), shackle:socket_options()) ->
    {ok, shackle:socket()} | {error, atom()}.

connect(Address, Port, SocketOptions) ->
    Timestamp = erlang:monotonic_time(millisecond),
    case shackle_socket:connect(Address, Port, tcp_opts(SocketOptions)) of
        {ok, Socket} ->
            handshake(Socket, ssl_opts(SocketOptions), Timestamp);
        {error, _} = Error ->
            Error
    end.

-spec send(shackle:socket(), iodata()) ->
    ok | {error, atom()}.

send(Socket, Data) ->
    shackle_socket:send(Socket, Data).

-spec setopts(shackle:socket(), [gen_tcp:option()]) ->
    ok |
    {error, atom()}.

setopts(Socket, Opts) ->
    shackle_socket:setopts(Socket, Opts).

%% private
handshake(Socket, SslOptions, Timestamp) ->
    Elapsed = erlang:monotonic_time(millisecond) - Timestamp,
    Timeout = max(?DEFAULT_CONNECT_TIMEOUT - Elapsed, 1),
    case ssl:connect(Socket, SslOptions, Timeout) of
        {ok, SslSocket} ->
            handover(SslSocket);
        {error, Reason} ->
            _ = socket:close(Socket),
            {error, error_reason(Reason)}
    end.

%% The connection pid is the connection_handler field of #sslsocket{}.
handover(SslSocket) ->
    case ssl_gen_statem:ktls_handover(element(3, SslSocket)) of
        {ok, #{socket := Socket} = Info} ->
            case maps:get(user_data, Info, <<>>) of
                <<>> ->
                    set_ktls(Socket, Info);
                _ ->
                    _ = socket:close(Socket),
                    {error, ktls_pending_data}
            end;
        {error, Reason} ->
            _ = ssl:close(SslSocket),
            {error, error_reason(Reason)}
    end.

set_ktls(Socket, Info) ->
    case inet_tls_dist:set_ktls(Info#{
        setopt_fun => fun setopt_native/3,
        getopt_fun => fun getopt_native/3
    }) of
        ok ->
            %% TLS_RX_EXPECT_NO_PAD (Linux 6.0+) decrypts into the user
            %% buffer; a padded record only costs a retry.
            _ = socket:setopt_native(Socket, {?SOL_TLS, ?TLS_RX_EXPECT_NO_PAD}, 1),
            {ok, Socket};
        {error, _} ->
            _ = socket:close(Socket),
            {error, ktls_unavailable}
    end.

setopt_native(Socket, Option, Value) ->
    socket:setopt_native(Socket, Option, Value).

getopt_native(Socket, Option, Size) ->
    socket:getopt_native(Socket, Option, Size).

tcp_opts(SocketOptions) ->
    [Opt || {Key, _} = Opt <- SocketOptions, lists:member(Key, tcp_keys())].

ssl_opts(SocketOptions) ->
    [binary, {active, false}, {ktls, true}, {versions, ['tlsv1.3']},
     {ciphers, [?KTLS_CIPHER]} |
     [Opt || Opt <- SocketOptions, not lists:member(key(Opt), skip_keys())]].

skip_keys() ->
    [binary, list, active, packet, ktls, versions, ciphers | tcp_keys()].

tcp_keys() ->
    [buffer, nodelay, recbuf, sndbuf, send_timeout, send_timeout_close,
     keepalive, priority, tos].

key({Key, _}) -> Key;
key(Key) -> Key.

error_reason(Reason) when is_atom(Reason) ->
    Reason;
error_reason({tls_alert, {Alert, _Description}}) ->
    Alert;
error_reason({options, _}) ->
    invalid_options;
error_reason(_) ->
    ssl_error.
