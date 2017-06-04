-module(shackle_ssl_server).
-include("shackle_internal.hrl").

-compile(inline).
-compile({inline_size, 512}).

-export([
    start_link/4,
    init/3,
    handle_msg/2,
    terminate/2
]).

-record(state, {
    client           :: client(),
    ip               :: inet:ip_address() | inet:hostname(),
    name             :: server_name(),
    parent           :: pid(),
    pool_name        :: pool_name(),
    port             :: inet:port_number(),
    reconnect_state  :: undefined | reconnect_state(),
    socket           :: undefined | inet:socket(),
    socket_options   :: [ssl:connect_option()],
    timer_ref        :: undefined | reference()
}).

-type init_opts() :: {pool_name(), client(), client_options()}.
-type state() :: #state {}.

%% public
-spec start_link(server_name(), pool_name(), client(), client_options()) ->
    {ok, pid()}.

start_link(Name, PoolName, Client, ClientOptions) ->
    Args = {PoolName, Client, ClientOptions},
    metal:start_link(?MODULE, Name, Args).

-spec init(server_name(), pid(), init_opts()) ->
    no_return().

init(Name, Parent, Opts) ->
    {PoolName, Client, ClientOptions} = Opts,
    self() ! ?MSG_CONNECT,
    ok = shackle_backlog:new(Name),

    Ip = ?LOOKUP(ip, ClientOptions, ?DEFAULT_IP),
    Port = ?LOOKUP(port, ClientOptions),
    ReconnectState = shackle_utils:reconnect_state(ClientOptions),
    SocketOptions = ?LOOKUP(socket_options, ClientOptions,
        ?DEFAULT_SOCKET_OPTS),

    {ok, {#state {
        client = Client,
        ip = Ip,
        name = Name,
        parent = Parent,
        pool_name = PoolName,
        port = Port,
        reconnect_state = ReconnectState,
        socket_options = SocketOptions
    }, undefined}}.

-spec handle_msg(term(), {state(), client_state()}) ->
    {ok, term()}.

handle_msg(#cast {} = Cast, {#state {
        socket = undefined,
        name = Name
    } = State, ClientState}) ->

    shackle_utils:reply(Name, {error, no_socket}, Cast),
    {ok, {State, ClientState}};
handle_msg(#cast {
        request = Request,
        timeout = Timeout
    } = Cast, {#state {
        client = Client,
        name = Name,
        pool_name = PoolName,
        socket = Socket
    } = State, ClientState}) ->

    {ok, ExtRequestId, Data, ClientState2} =
        Client:handle_request(Request, ClientState),

    case ssl:send(Socket, Data) of
        ok ->
            Msg = {timeout, ExtRequestId},
            TimerRef = erlang:send_after(Timeout, self(), Msg),
            shackle_queue:add(ExtRequestId, Cast, TimerRef),
            {ok, {State, ClientState2}};
        {error, Reason} ->
            shackle_utils:warning_msg(PoolName, "send error: ~p", [Reason]),
            ssl:close(Socket),
            shackle_utils:reply(Name, {error, socket_closed}, Cast),
            close(State, ClientState2)
    end;
handle_msg({ssl, _Socket, Data}, {#state {
        client = Client,
        name = Name,
        pool_name = PoolName,
        socket = Socket
    } = State, ClientState}) ->

    case Client:handle_data(Data, ClientState) of
        {ok, Replies, ClientState2} ->
            shackle_utils:process_responses(Replies, Name),
            {ok, {State, ClientState2}};
        {error, Reason, ClientState2} ->
            shackle_utils:warning_msg(PoolName,
                "handle_data error: ~p", [Reason]),
            ssl:close(Socket),
            close(State, ClientState2)
    end;
handle_msg({timeout, ExtRequestId}, {#state {
        name = Name
    } = State, ClientState}) ->

    case shackle_queue:remove(Name, ExtRequestId) of
        {ok, Cast, _TimerRef} ->
            shackle_utils:reply(Name, {error, timeout}, Cast);
        {error, not_found} ->
            ok
    end,
    {ok, {State, ClientState}};
handle_msg({ssl_closed, Socket}, {#state {
        socket = Socket,
        pool_name = PoolName
    } = State, ClientState}) ->

    shackle_utils:warning_msg(PoolName, "connection closed", []),
    close(State, ClientState);
handle_msg({ssl_error, Socket, Reason}, {#state {
        socket = Socket,
        pool_name = PoolName
    } = State, ClientState}) ->

    shackle_utils:warning_msg(PoolName, "connection error: ~p", [Reason]),
    ssl:close(Socket),
    close(State, ClientState);
handle_msg(?MSG_CONNECT, {#state {
        client = Client,
        ip = Ip,
        pool_name = PoolName,
        port = Port,
        reconnect_state = ReconnectState,
        socket_options = SocketOptions
    } = State, ClientState}) ->

    case connect(PoolName, Ip, Port, SocketOptions) of
        {ok, Socket} ->
            case client_setup(Client, PoolName, Socket) of
                {ok, ClientState2} ->
                    ReconnectState2 =
                        shackle_utils:reconnect_state_reset(ReconnectState),

                    {ok, {State#state {
                        reconnect_state = ReconnectState2,
                        socket = Socket
                    }, ClientState2}};
                {error, _Reason, ClientState2} ->
                    ssl:close(Socket),
                    reconnect(State, ClientState2)
            end;
        {error, _Reason} ->
            reconnect(State, ClientState)
    end.

-spec terminate(term(), term()) ->
    ok.

terminate(_Reason, {#state {
        client = Client,
        name = Name,
        timer_ref = TimerRef
    }, ClientState}) ->

    shackle_utils:cancel_timer(TimerRef),
    ok = Client:terminate(ClientState),
    shackle_utils:reply_all(Name, {error, shutdown}),
    shackle_backlog:delete(Name),
    ok.

%% private
client_setup(Client, PoolName, Socket) ->
    {ok, ClientState} = Client:init(),
    ssl:setopts(Socket, [{active, false}]),
    case Client:setup(Socket, ClientState) of
        {ok, ClientState2} ->
            ssl:setopts(Socket, [{active, true}]),
            {ok, ClientState2};
        {error, Reason, ClientState2} ->
            shackle_utils:warning_msg(PoolName,
                "setup error: ~p", [Reason]),
            {error, Reason, ClientState2}
    end.

close(#state {name = Name} = State, ClientState) ->
    shackle_utils:reply_all(Name, {error, socket_closed}),
    reconnect(State, ClientState).

connect(PoolName, Ip, Port, SocketOptions) ->
    case inet:getaddrs(Ip, inet) of
        {ok, Addrs} ->
            Ip2 = shackle_utils:random_element(Addrs),
            case ssl:connect(Ip2, Port, SocketOptions,
                    ?DEFAULT_CONNECT_TIMEOUT) of
                {ok, Socket} ->
                    {ok, Socket};
                {error, Reason} ->
                    shackle_utils:warning_msg(PoolName,
                        "connect error: ~p", [Reason]),
                    {error, Reason}
            end;
        {error, Reason} ->
            shackle_utils:warning_msg(PoolName,
                "getaddrs error: ~p", [Reason]),
            {error, Reason}
    end.

reconnect(State, undefined) ->
    reconnect_timer(State, undefined);
reconnect(#state {
        client = Client
    } = State, ClientState) ->

    ok = Client:terminate(ClientState),
    reconnect_timer(State, ClientState).

reconnect_timer(#state {
        reconnect_state = undefined
    } = State, ClientState) ->

    {ok, {State#state {
        socket = undefined
    }, ClientState}};
reconnect_timer(#state {
        reconnect_state = ReconnectState
    } = State, ClientState)  ->

    ReconnectState2 = shackle_backoff:timeout(ReconnectState),
    #reconnect_state {current = Current} = ReconnectState2,
    TimerRef = erlang:send_after(Current, self(), ?MSG_CONNECT),

    {ok, {State#state {
        reconnect_state = ReconnectState2,
        socket = undefined,
        timer_ref = TimerRef
    }, ClientState}}.
