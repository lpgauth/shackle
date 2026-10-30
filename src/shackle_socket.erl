-module(shackle_socket).
-moduledoc """
NIF-based TCP socket protocol using the socket module (OTP 27.3+).

This protocol uses Erlang's socket module with OTP select read support for
improved performance compared to gen_tcp. It provides:

- Better control over socket options
- socket:send/2 for iodata (OTP 27+)
- Requires OTP 27.3 or later

Reads are driven by select notifications. When the socket becomes readable,
the owner process receives `{'$socket', Socket, select, Ref}`. shackle_server
then calls recv/1, which delivers the data to the client's handle_data/2 as if
it had arrived as a `{tcp, Socket, Data}` message.

Falls back to gen_tcp on older OTP versions.
""".
-include("shackle_defaults.hrl").

-dialyzer({nowarn_function, [recv/1, send/2, sendv/2, setopts/2]}).
-ignore_xref([
    {socket, sendv, 2}
]).

-behavior(shackle_protocol).
-export([
    close/1,
    connect/3,
    send/2,
    setopts/2
]).

%% internal
-export([
    recv/1
]).

%% callbacks
-spec close(shackle:socket()) ->
    ok.

close(Socket) ->
    _ = socket:close(Socket),
    ok.

-spec connect(shackle:inet_address(), shackle:inet_port(), shackle:socket_options()) ->
    {ok, shackle:socket()} | {error, atom()}.

connect(Address, Port, SocketOptions) ->
    case socket:open(inet, stream, tcp) of
        {ok, Socket} ->
            case connect_opts(Socket, SocketOptions) of
                ok ->
                    SockAddr = #{family => inet, addr => Address, port => Port},
                    case socket:connect(Socket, SockAddr,
                        ?DEFAULT_CONNECT_TIMEOUT) of

                        ok ->
                            {ok, Socket};
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

%% Reads whatever is available; with {otp, select_read} enabled the
%% successful recv re-arms the read select inside the same NIF call.
-spec recv(shackle:socket()) ->
    {ok, binary()} | wait | {error, atom()}.

recv(Socket) ->
    case socket:recv(Socket, 0, [], nowait) of
        {select_read, {_SelectInfo, Data}} ->
            {ok, Data};
        {select, {_SelectInfo, Data}} ->
            {ok, Data};
        {select, _SelectInfo} ->
            wait;
        {ok, Data} ->
            %% Data was read but the select may not be re-armed; callers keep
            %% calling recv/1 until it returns `wait', which arms it.
            {ok, Data};
        {error, {Reason, _Data}} ->
            {error, Reason};
        {error, Reason} ->
            {error, Reason}
    end.

-spec send(shackle:socket(), iodata()) ->
    ok | {error, atom()}.

send(Socket, Data) when is_binary(Data) ->
    case socket:send(Socket, Data) of
        ok ->
            ok;
        {error, {Reason, _RestData}} ->
            {error, Reason};
        {error, Reason} ->
            {error, Reason}
    end;
%% socket:send flattens iolists with list_to_binary; sendv keeps the
%% iovec and writes it in one NIF call
send(Socket, Data) ->
    sendv(Socket, erlang:iolist_to_iovec(Data)).

-spec setopts(shackle:socket(), [gen_tcp:option()]) ->
    ok |
    {error, atom()}.

setopts(Socket, [{active, false}]) ->
    _ = socket:setopt(Socket, {otp, select_read}, false),
    ok;
setopts(Socket, [{active, true}]) ->
    ok = socket:setopt(Socket, {otp, select_read}, true),
    arm_and_deliver(Socket);
setopts(Socket, Opts) ->
    connect_opts(Socket, Opts).

%% private
%% Reads until recv/1 returns `wait', which leaves the select armed. Each chunk
%% is sent to the owner like an active-mode packet.
arm_and_deliver(Socket) ->
    case recv(Socket) of
        {ok, Data} ->
            self() ! {tcp, Socket, Data},
            arm_and_deliver(Socket);
        wait ->
            ok;
        {error, _} = Error ->
            Error
    end.

sendv(Socket, IOV) ->
    case socket:sendv(Socket, IOV) of
        ok ->
            ok;
        {ok, RestIOV} ->
            sendv(Socket, RestIOV);
        {error, {Reason, _RestIOV}} ->
            {error, Reason};
        {error, Reason} ->
            {error, Reason}
    end.

connect_opts(_Socket, []) ->
    ok;
connect_opts(Socket, [{nodelay, Bool} | T]) ->
    case socket:setopt(Socket, {tcp, nodelay}, Bool) of
        ok ->
            connect_opts(Socket, T);
        {error, _} = Error ->
            Error
    end;
connect_opts(Socket, [{recbuf, Size} | T]) ->
    case socket:setopt(Socket, {socket, rcvbuf}, Size) of
        ok ->
            connect_opts(Socket, T);
        {error, _} = Error ->
            Error
    end;
connect_opts(Socket, [{sndbuf, Size} | T]) ->
    case socket:setopt(Socket, {socket, sndbuf}, Size) of
        ok ->
            connect_opts(Socket, T);
        {error, _} = Error ->
            Error
    end;
connect_opts(Socket, [_ | T]) ->
    connect_opts(Socket, T).