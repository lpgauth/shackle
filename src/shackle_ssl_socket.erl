-module(shackle_ssl_socket).
-include("shackle_internal.hrl").

%% TLS over the socket NIF: connect/3 opens and connects a raw
%% socket:socket(), applies the inet-level options to it, then hands
%% it to ssl:connect/3, which selects the tls_socket_tcp transport
%% for {'$socket', _} sockets. That transport lands in OTP 28.0
%% (send-side buffering in 28.1, recommended) and the ssl specs
%% before 28 don't accept socket:socket(), so OTP 28 is the runtime
%% floor for this protocol.
-dialyzer({nowarn_function, [connect/3, error_reason/1, handshake/3]}).

-behavior(shackle_protocol).
-export([
    close/1,
    connect/3,
    send/2,
    setopts/2
]).

%% callbacks
-spec close(shackle:socket()) ->
    ok.

close(Socket) ->
    ssl:close(Socket),
    ok.

-spec connect(shackle:inet_address(), shackle:inet_port(), shackle:socket_options()) ->
    {ok, shackle:socket()} | {error, atom()}.

connect(Address, Port, SocketOptions) ->
    Timestamp = erlang:monotonic_time(millisecond),
    case socket:open(inet, stream, tcp) of
        {ok, Socket} ->
            case connect_opts(Socket, SocketOptions, []) of
                {ok, SslOptions} ->
                    SockAddr = #{
                        family => inet,
                        addr => Address,
                        port => Port
                    },
                    case socket:connect(Socket, SockAddr,
                        ?DEFAULT_CONNECT_TIMEOUT) of

                        ok ->
                            handshake(Socket, SslOptions, Timestamp);
                        {error, Reason} ->
                            _ = socket:close(Socket),
                            {error, Reason}
                    end;
                {error, Reason} ->
                    _ = socket:close(Socket),
                    {error, Reason}
            end;
        {error, _} = Error ->
            Error
    end.

-spec send(shackle:socket(), iodata()) ->
    ok | {error, atom()}.

send(Socket, Data) ->
    ssl:send(Socket, Data).

-spec setopts(shackle:socket(), [gen_tcp:option()]) ->
    ok |
    {error, atom()}.

setopts(Socket, Opts) ->
    ssl:setopts(Socket, Opts).

%% private
%% nodelay/recbuf/sndbuf are applied to the raw socket before the
%% handshake; everything else (mode, packet, tls options) goes to
%% ssl:connect, which emulates the inet options.
connect_opts(_Socket, [], Acc) ->
    {ok, lists:reverse(Acc)};
connect_opts(Socket, [{nodelay, Bool} | T], Acc) ->
    case socket:setopt(Socket, {tcp, nodelay}, Bool) of
        ok ->
            connect_opts(Socket, T, Acc);
        {error, _} = Error ->
            Error
    end;
connect_opts(Socket, [{recbuf, Size} | T], Acc) ->
    case socket:setopt(Socket, {socket, rcvbuf}, Size) of
        ok ->
            connect_opts(Socket, T, Acc);
        {error, _} = Error ->
            Error
    end;
connect_opts(Socket, [{sndbuf, Size} | T], Acc) ->
    case socket:setopt(Socket, {socket, sndbuf}, Size) of
        ok ->
            connect_opts(Socket, T, Acc);
        {error, _} = Error ->
            Error
    end;
connect_opts(Socket, [Opt | T], Acc) ->
    connect_opts(Socket, T, [Opt | Acc]).

error_reason(Reason) when is_atom(Reason) ->
    Reason;
error_reason({tls_alert, {Alert, _Description}}) ->
    Alert;
error_reason({options, _}) ->
    invalid_options;
error_reason(_) ->
    ssl_error.

%% The remaining connect budget goes to the handshake so a backend
%% swap from shackle_ssl keeps the same total connect deadline.
handshake(Socket, SslOptions, Timestamp) ->
    Elapsed = erlang:monotonic_time(millisecond) - Timestamp,
    Timeout = max(?DEFAULT_CONNECT_TIMEOUT - Elapsed, 1),
    case ssl:connect(Socket, SslOptions, Timeout) of
        {ok, SslSocket} ->
            {ok, SslSocket};
        {error, Reason} ->
            _ = socket:close(Socket),
            {error, error_reason(Reason)}
    end.
