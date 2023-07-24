-module(shackle).
-include("shackle_internal.hrl").

-compile(inline).
-compile({inline_size, 512}).

%% public
-export([
    call/2,
    call/3,
    cast/2,
    cast/3,
    cast/4,
    receive_response/1
]).

%% types
-type cast() :: #cast {}.
-type client() :: module().
-type external_request_id() :: term().
-type inet_address() :: inet:ip_address() | inet:hostname().
-type inet_port() :: inet:port_number().
-type protocol() :: shackle_ssl| shackle_tcp | shackle_udp.
-type request_id() :: {shackle_server:name(), reference()}.
-type response() :: {external_request_id(), term()}.
-type socket() :: inet:socket() | ssl:sslsocket().
-type socket_option() :: gen_tcp:connect_option() | gen_udp:option() | ssl:tls_client_option().
-type socket_options() :: [socket_option()].
-type table() :: atom().
-type time() :: pos_integer().

-export_type([
    cast/0,
    client/0,
    external_request_id/0,
    inet_address/0,
    inet_port/0,
    protocol/0,
    request_id/0,
    response/0,
    socket/0,
    socket_options/0,
    table/0,
    time/0
]).

%% public
-spec call(shackle_pool:name(), term()) ->
    term() | {error, term()}.

call(PoolName, Request) ->
    call(PoolName, Request, ?DEFAULT_TIMEOUT).

-spec call(atom(), term(), timeout()) ->
    term() | {error, atom()}.

call(PoolName, Request, Timeout) ->
    case cast(PoolName, Request, self(), Timeout) of
        {ok, RequestId} ->
            receive_response(RequestId);
        {error, Reason} ->
            {error, Reason}
    end.

-spec cast(shackle_pool:name(), term()) ->
    {ok, request_id()} | {error, atom()}.

cast(PoolName, Request) ->
    cast(PoolName, Request, self()).

-spec cast(shackle_pool:name(), term(), undefined | pid()) ->
    {ok, request_id()} | {error, atom()}.

cast(PoolName, Request, Pid) ->
    cast(PoolName, Request, Pid, ?DEFAULT_TIMEOUT).

-spec cast(shackle_pool:name(), term(), undefined | pid(), timeout()) ->
    {ok, request_id()} | {error, atom()}.

cast(PoolName, Request, Pid, Timeout) ->
    Timestamp = os:timestamp(),
    Ref = make_ref(),
    case shackle_pool:server(PoolName) of
        {ok, Client, Server} ->
            RequestId = {Server, Ref},
            Server ! {Request, #cast {
                client = Client,
                pid = Pid,
                request_id = RequestId,
                timeout = Timeout,
                timestamp = Timestamp
            }},
            {ok, RequestId};
        {error, Reason} ->
            {error, Reason}
    end.

-spec receive_response(request_id()) ->
    term() | {error, term()}.

receive_response(RequestId) ->
    receive
        {#cast {request_id = RequestId}, Reply} ->
            Reply
    end.
