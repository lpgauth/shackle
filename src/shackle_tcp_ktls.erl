-module(shackle_tcp_ktls).
-include("shackle_internal.hrl").

%% TLS 1.3 with kernel TLS: ssl does the handshake over gen_tcp, then
%% hands the record keys to the kernel and exits, so the connection
%% carries plaintext like shackle_tcp. Linux only, needs the `tls`
%% kernel module and OTP 26+, and OTP only maps TLS_AES_256_GCM_SHA384
%% to kernel cipher parameters, so the handshake is pinned to it.
-ignore_xref([{ssl_gen_statem, ktls_handover, 1},
              {inet_tls_dist, set_ktls, 1},
              {inet_tls_dist, inet_ktls_setopt, 3},
              {inet_tls_dist, inet_ktls_getopt, 3}]).
-dialyzer({nowarn_function, [connect/3, handover/2]}).

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
    gen_tcp:close(Socket).

-spec connect(shackle:inet_address(), shackle:inet_port(), shackle:socket_options()) ->
    {ok, shackle:socket()} | {error, atom()}.

connect(Address, Port, SocketOptions) ->
    Timestamp = erlang:monotonic_time(millisecond),
    case gen_tcp:connect(Address, Port, tcp_opts(SocketOptions),
        ?DEFAULT_CONNECT_TIMEOUT) of

        {ok, Socket} ->
            Elapsed = erlang:monotonic_time(millisecond) - Timestamp,
            Timeout = max(?DEFAULT_CONNECT_TIMEOUT - Elapsed, 1),
            case ssl:connect(Socket, ssl_opts(SocketOptions), Timeout) of
                {ok, SslSocket} ->
                    handover(SslSocket, SocketOptions);
                {error, Reason} ->
                    _ = gen_tcp:close(Socket),
                    {error, error_reason(Reason)}
            end;
        {error, _} = Error ->
            Error
    end.

-spec send(shackle:socket(), iodata()) ->
    ok | {error, atom()}.

send(Socket, Data) ->
    gen_tcp:send(Socket, Data).

-spec setopts(shackle:socket(), [gen_tcp:option()]) ->
    ok |
    {error, atom()}.

setopts(Socket, Opts) ->
    inet:setopts(Socket, Opts).

%% private
%% The connection pid is the connection_handler field of #sslsocket{}.
handover(SslSocket, SocketOptions) ->
    case ssl_gen_statem:ktls_handover(element(3, SslSocket)) of
        {ok, #{socket := Socket} = Info} ->
            case inet_tls_dist:set_ktls(Info#{
                setopt_fun => fun inet_tls_dist:inet_ktls_setopt/3,
                getopt_fun => fun inet_tls_dist:inet_ktls_getopt/3
            }) of
                ok ->
                    %% TLS_RX_EXPECT_NO_PAD (Linux 6.0+) decrypts into the
                    %% user buffer; a padded record only costs a retry.
                    _ = inet:setopts(Socket, [{raw, ?SOL_TLS, ?TLS_RX_EXPECT_NO_PAD,
                                               <<1:32/native>>}]),
                    case inet:setopts(Socket, user_opts(SocketOptions)) of
                        ok ->
                            {ok, Socket};
                        {error, Reason} ->
                            _ = gen_tcp:close(Socket),
                            {error, Reason}
                    end;
                {error, _} ->
                    _ = gen_tcp:close(Socket),
                    {error, ktls_unavailable}
            end;
        {error, Reason} ->
            _ = ssl:close(SslSocket),
            {error, error_reason(Reason)}
    end.

tcp_opts(SocketOptions) ->
    [binary, {active, false}, {packet, raw} |
     [Opt || {Key, _} = Opt <- SocketOptions, lists:member(Key, tcp_keys())]].

ssl_opts(SocketOptions) ->
    Opts = [Opt || Opt <- SocketOptions, not is_inet_opt(Opt)],
    [binary, {active, false}, {ktls, true}, {versions, ['tlsv1.3']},
     {ciphers, [?KTLS_CIPHER]} |
     [Opt || Opt <- Opts, not lists:member(key(Opt), [ktls, versions, ciphers])]].

user_opts(SocketOptions) ->
    Mode = case lists:member(list, SocketOptions) of
        true -> list;
        false -> binary
    end,
    [Mode, {active, false}, {packet, proplists:get_value(packet, SocketOptions, raw)}].

is_inet_opt(Opt) ->
    lists:member(key(Opt), [binary, list, active, packet | tcp_keys()]).

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
