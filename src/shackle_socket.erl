-module(shackle_socket).
-include("shackle_internal.hrl").

%% The socket specs on OTP < 28 predate {otp, select_read} and its
%% recv return shapes, and socket:sendv only exists from OTP 27,
%% although everything works at runtime from OTP 27.3.
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
            %% select_read did not re-arm; force another recv round
            self() ! {'$socket', Socket, select, undefined},
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
    case recv(Socket) of
        {ok, Data} ->
            %% shackle_server delivers this to the client like any
            %% active-mode packet
            self() ! {tcp, Socket, Data},
            ok;
        wait ->
            ok;
        {error, _} = Error ->
            Error
    end;
setopts(Socket, Opts) ->
    connect_opts(Socket, Opts).

%% private
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
