%% @doc A process handling a pool of client connections to a server.
%%
%% Configuration options:
%% <du>
%% <dt>service_name</dt>
%%  <dd>Name of the endpoint service (defaults to the client module name)</dd>
%% <dt>init_options</dt>
%%  <dd>Options passed to the client in the `init/3' callback.</dd>
%% <dt>address</dt><dd>IP address of the server's endpoint to connect to.</dd>
%% <dt>port</dt><dd>Server port to connect to.</dd>
%% <dt>protocol</dt>
%%  <dd>Connection protocol `shackle_ssl | shackle_tcp | shackle_udp'
%%      (default: `shackle_tcp').</dd>
%% <dt>reconnect</dt>
%%  <dd>When true (default), automatically reconnect upon a disconnect.</dd>
%% <dt>reconnect_time_min</dt>
%%  <dd>Min reconnect time in milliseconds (default: `500').</dd>
%% <dt>reconnect_time_max</dt>
%%  <dd>Max reconnect time in milliseconds (default: `120000').</dd>
%% <dt>socket_options</dt><dd>Options passed to the socket (default: `[]')</dd>
%% <dt>bounce_interval_secs</dt>
%%  <dd>Interval in seconds when a connection must be forcefully bounced.
%%      Defaults to `infinity', which disables the bouncing feature. A
%%      bounce is done on one connection at a time.</dd>
%% <dt>bounce_udp</dt>
%%  <dd>When true and bounce_interval_secs is an integer, will allow
%%      bouncing of UDP connections</dd>
%% <dt>on_bounce_event</dt>
%%  <dd>A callback function called on various events related to connection
%%      bouncing.</dd>
%% </du>
-module(shackle_server).
-include("shackle_internal.hrl").

-compile(inline).
-compile({inline_size, 512}).

-export([
    start_link/2,
    state/1,
    state/2,
    bounce/1,
    bounce/2,
    next_bounce/1,
    next_bounce/2,
    set_bounce_event/2,
    set_bounce_event/3
]).

-behaviour(metal).
-export([
    init/3,
    handle_msg/2,
    terminate/2
]).

-ifndef(NO_BOUNCE_EVENT).
-define(ON_BOUNCE_EVENT(State, Event),
    (is_function(State#state.on_bounce_event, 3) andalso
        (State#state.on_bounce_event)(
            element(1, State#state.id), element(2, State#state.id), Event))).
-else.
-define(ON_BOUNCE_EVENT(_State, _Event), ok).
-endif.

-record(state, {
    address          :: shackle:inet_address(),
    client           :: shackle:client(),
    id               :: id(),
    srv_idx          :: binary(),
    init_options     :: init_options(),
    name             :: name(),
    service_name     :: binary(),
    parent           :: pid(),
    pool_name        :: shackle_pool:name(),
    port             :: shackle:inet_port(),
    protocol         :: shackle:protocol(),
    queue            :: #{shackle:external_request_id() => shackle:cast()},
    reconnect_state  :: undefined | reconnect_state(),
    servers          :: undefined | shackle_servers:state(),  %% Multi-server failover state
    socket           :: undefined | shackle:socket(),
    socket_options   :: shackle:socket_options(),
    bounce_interval  :: infinity  | pos_integer(),  %% # of ms for conn bounce
    bounce_state     :: waiting   |                 %% Waiting for next bounce
                        draining  |                 %% Draining request queue
                        awaiting_empty_queue |      %% Awaiting for the request queue to empty
                        reconnecting |              %% Reconnecting to server
                        disconnected,               %% Disconnected from server
    timer_ref        :: undefined | reference(),
    bounce_timer_ref :: undefined | reference(),
    drain_timer_ref  :: undefined | reference(),    %% Timer reference of queue drain check
    on_bounce_event  :: undefined | function(),
    next_bounce      :: undefined | pos_integer(),  %% Epoch timestamp when to close connection
    trace_log        :: undefined | term(),
    sock_id          :: integer()
}).

-ifndef(WITH_SHACKLE_TRACE_LOG).

-else.
-endif.

-type state() :: #state {}.
-type client_state() :: term().
-type init_options() :: term().
-type id() :: {shackle_pool:name(), index()}.
-type index() :: pos_integer().
-type name() :: atom().
-type opts() :: {shackle_pool:name(), index(), shackle:client(), shackle_client:options()}.
-type reconnect_state() :: #reconnect_state{}.

-export_type([
    id/0,
    index/0,
    init_options/0,
    name/0,
    reconnect_state/0
]).

%% Queue internal API

queue_new() ->
    #{}.

queue_add(Key, Value, #state{queue = Queue} = State) ->
    State#state{queue = Queue#{Key => Value}}.

queue_find(Key, #state{queue = Queue}) ->
    maps:find(Key, Queue).

queue_take(Key, #state{queue = Queue} = State) ->
    case maps:take(Key, Queue) of
        {Value, Queue_} ->
            {Value, State#state{queue = Queue_}};
        error ->
            error
    end.

queue_remove(Key, #state{queue = Queue} = State) ->
    State#state{queue = maps:remove(Key, Queue)}.

queue_update(Key, Value, #state{queue = Queue} = State) ->
    State#state{queue = maps:update(Key, Value, Queue)}.

queue_clear(#state{queue = Queue} = State) ->
    {maps:values(Queue), State#state{queue = #{}}}.

queue_empty(#state{queue = Queue}) ->
    Queue =:= #{}.

%% public
-spec start_link(name(), opts()) ->
    {ok, pid()}.

start_link(Name, Opts) ->
    metal:start_link(?MODULE, Name, Opts).

-spec state(shackle_pool:name(), pos_integer()) ->
    {ok, #{connection_state => map(), handler_state => any()}} | {error, noproc}.
state(PoolName, SrvIdx) ->
    state(shackle_pool:server_name(PoolName, SrvIdx)).

-spec state(atom()) ->
    {ok, #{connection_state => map(), handler_state => any()}} | {error, noproc}.
state(SrvName) ->
    try sys:get_state(SrvName) of
        {?MODULE, _SrvName, _Pid, {State, CliState}} ->
            {ok, #{
                connection_state =>
                    maps:from_list(lists:zip(
                        record_info(fields, state),
                        tl(tuple_to_list(State))
                    )),
                handler_state => CliState}
            }
    catch error:noproc ->
        {error, noproc}
    end.

-spec bounce(shackle_pool:name(), pos_integer()) -> boolean().
bounce(PoolName, SrvIdx) ->
    bounce(shackle_pool:server_name(PoolName, SrvIdx)).

-spec bounce(atom()) -> boolean().
bounce(SrvName) ->
    try
        SrvName ! bounce_connection,
        true
    catch
        error:badarg -> false
    end.

-spec next_bounce(shackle_pool:name(), pos_integer()) ->
    {ok, integer() | infinity} | {error, any}.
next_bounce(PoolName, SrvIdx) ->
    next_bounce(shackle_pool:server_name(PoolName, SrvIdx)).

%% Get the number of milliseconds left until the next connection bounce
-spec next_bounce(atom()) ->
    {ok, integer() | infinity} | {error, any}.
next_bounce(SrvName) ->
    try
        Ref = make_ref(),
        SrvName ! {next_bounce, self(), Ref},
        receive
            {Ref, Interval} ->
                {ok, Interval}
        after 5000 ->
            {error, timeout}
        end
    catch _:R ->
        {error, R}
    end.

%% @doc Set the bounce event callback.
%% Use `Fun = undefiend` to clear the bounce event.  The bounce event
%% is only available if the project is compiled without the `NO_BOUNCE_EVENT'
%% option.
-spec set_bounce_event(shackle_pool:name(), pos_integer(),
                        fun((atom(), integer(), map()) -> ok) | undefined) ->
    ok | not_found.
set_bounce_event(PoolName, SrvIdx, Fun) ->
    set_bounce_event(shackle_pool:server_name(PoolName, SrvIdx), Fun).

-spec set_bounce_event(atom(), fun((atom(), integer(), map()) -> ok) | undefined) ->
    ok | not_found.
set_bounce_event(SrvName, Fun) when is_function(Fun, 3); Fun == undefined ->
    try
        SrvName ! {set_bounce_event, Fun},
        ok
    catch _ ->
        not_found
    end.

%% metal callbacks
-spec init(name(), pid(), opts()) ->
    {ok, {state(), term()}}.

init(Name, Parent, Opts) ->
    {PoolName, Index, Client, ServerOpts} = Opts,
    ensure_loaded(Client),
    ServerOpts1 = shackle_utils:default_options(client, ServerOpts),

    self() ! ?MSG_CONNECT,
    Id = {PoolName, Index},
    SrvIdxBin = integer_to_binary(Index),
    InitOptions = ?LOOKUP(init_options, ServerOpts1, ?DEFAULT_INIT_OPTS),
    Address = address(ServerOpts1),
    Port = ?LOOKUP(port, ServerOpts1),
    Protocol = ?LOOKUP(protocol, ServerOpts1, ?DEFAULT_PROTOCOL),
    SocketOptions = ?LOOKUP(socket_options, ServerOpts1, ?DEFAULT_SOCKET_OPTS),
    ServiceName = to_bin(?LOOKUP(service_name, ServerOpts1, Client)),
    ReconnectState = reconnect_state(ServerOpts1),
    TraceLog = init_trace_log(),
    BounceUDP = ?LOOKUP(bounce_udp, ServerOpts1, ?DEFAULT_BOUNCE_UDP),
    BounceInt =
        case ?LOOKUP(bounce_interval_secs, ServerOpts1, ?DEFAULT_BOUNCE_INTERVAL) of
            _ when Protocol == shackle_udp, BounceUDP ->
                infinity;
            I when is_integer(I) ->
                I * 1000;
            infinity = I ->
                I
        end,
    OnBounceEvent =
        case ?LOOKUP(on_bounce_event, ServerOpts1, undefined) of
            undefined ->
                undefined;
            Fun when is_function(Fun, 3) ->
                Fun;
            {M, F} when is_atom(M), is_atom(F) ->
                ensure_loaded(M),
                erlang:function_exported(M, F, 3)
                    orelse error({function_not_exported, {M, F, 3}}),
                fun(Event) -> M:F(PoolName, Index, Event) end
        end,

    % Initialize multi-server state if servers list is provided
    ServersState = case ?LOOKUP(servers, ServerOpts1, undefined) of
        undefined ->
            % Fallback to single address/port
            undefined;
        ServersList ->
            shackle_servers:new(ServersList, Port)
    end,

    lists:member(Port, [undefined, 0]) andalso
        erlang:error({missing_port_option, Name, ServerOpts1}),

    {ok, {#state {
        address = Address,
        client = Client,
        id = Id,
        srv_idx = SrvIdxBin,
        init_options = InitOptions,
        name = Name,
        service_name = ServiceName,
        parent = Parent,
        pool_name = PoolName,
        port = Port,
        protocol = Protocol,
        queue = queue_new(),
        reconnect_state = ReconnectState,
        servers = ServersState,
        socket_options = SocketOptions,
        bounce_interval = BounceInt,
        on_bounce_event = OnBounceEvent,
        bounce_state = waiting,
        trace_log = TraceLog,
        sock_id = 0
    }, undefined}}.

-spec handle_msg(term(), {state(), client_state()}) ->
    {ok, term()}.

handle_msg({request, [Casts | _]}, {#state {socket = undefined} = S, CliState}) ->
    inc_metrics(S, shackle_error_total, <<"no socket">>),
    reply({error, no_socket}, wrap(Casts), S, CliState);

handle_msg({request, [Casts | _]}, {#state{bounce_state = BS} = S, CliState}) when BS /= waiting ->
    %% The server is either in the connection draining state or about to be
    %% bounced - reject the client's request.
    inc_metrics(S, shackle_error_total, <<"send rejected">>),
    reply({error, send_rejected}, wrap(Casts), S, CliState);

handle_msg({request, [#cast {timeout = _Timeout} = Cast, Request]},
        {#state{bounce_state = waiting, client = Client} = State, ClientState}) ->
    try client_handle_request(Client, Cast, Request, ClientState) of
        {ok, Cast2, ExtRequestId, Data, ClientState2} ->
            Protocol = State#state.protocol,
            Socket = State#state.socket,
            case Protocol:send(Socket, Data) of
                ok ->
                    inc_metrics(State, shackle_request_total),
                    case ExtRequestId of
                        undefined ->
                            reply(ok, [Cast2], State, ClientState2);
                        _ ->
                            State2 = set_receive_timeout(State, ExtRequestId, Cast2),
                            {ok, {State2, ClientState2}}
                    end;
                {error, Reason} ->
                    Client = State#state.client,
                    PoolName = State#state.pool_name,
                    inc_metrics(State, shackle_error_total, <<"send error">>),
                    ?WARN(PoolName, "SrvIdx=~s send error: ~p", [State#state.srv_idx, Reason]),
                    {ok, {State3, ClientState3}} =
                        reply({error, socket_closed}, [Cast2], State, ClientState2),
                    close(State3, ClientState3)
            end
    catch
        ?EXCEPTION(E, R, Stacktrace) ->
            Client = State#state.client,
            PoolName = State#state.pool_name,
            inc_metrics(State, shackle_error_total, <<"handle_request error">>),
            ?WARN(PoolName, "handle_request crash: ~p:~p~n  ~p~n",
                [E, R, ?GET_STACK(Stacktrace)]),
            reply({error, client_crash}, [Cast], State, ClientState)
    end;

handle_msg({request, [Casts, Requests, Count]}, {#state {client = Client} = State, ClientState})
    when is_integer(Count), Count >= 0 ->
    try client_handle_requests(Client, Casts, Requests, ClientState) of
        {ok, Casts2, ExtRequestIds, Data, ClientState2} ->
            Protocol = State#state.protocol,
            Socket = State#state.socket,
            case Protocol:send(Socket, Data) of
                ok ->
                    inc_metrics(State, shackle_request_total, Count),
                    State2 = handle_request_ids_from_client(ExtRequestIds, Casts2, State, ClientState),
                    {ok, {State2, ClientState2}};
                {error, Reason} ->
                    inc_metrics(State, shackle_error_total, <<"send error">>),
                    ?WARN(State#state.pool_name, "SrvIdx=~s send error: ~p",
                        [State#state.srv_idx, Reason]),
                    {ok, {State3, ClientState3}} =
                        reply({error, socket_closed}, Casts2, State, ClientState2),
                    close(State3, ClientState3)
            end
    catch
        ?EXCEPTION(E, R, Stacktrace) ->
            inc_metrics(State, shackle_error_total, <<"handle_request error">>),
            ?WARN(State#state.pool_name, "handle_request crash: ~p:~p~n~p~n",
                [E, R, ?GET_STACK(Stacktrace)]),
                reply({error, client_crash}, Casts, State, ClientState)
    end;

handle_msg({ssl, Socket, Data}, {State, ClientState}) ->
    handle_msg_data(Socket, Data, State, ClientState);
handle_msg({ssl_closed, Socket}, {State, ClientState}) ->
    handle_msg_close(Socket, State, ClientState);
handle_msg({ssl_error, Socket, Reason}, {State, ClientState}) ->
    handle_msg_error(Socket, Reason, State, ClientState);
handle_msg({tcp, Socket, Data}, {State, ClientState}) ->
    report_socket_rtt(State),
    handle_msg_data(Socket, Data, State, ClientState);
handle_msg({tcp_closed, Socket}, {State, ClientState}) ->
    handle_msg_close(Socket, State, ClientState);
handle_msg({tcp_error, Socket, Reason}, {State, ClientState}) ->
    handle_msg_error(Socket, Reason, State, ClientState);
handle_msg({udp, Socket, _Ip, _InPortNo, Data}, {State, ClientState}) ->
    handle_msg_data(Socket, Data, State, ClientState);
handle_msg({udp_closed, Socket}, {State, ClientState}) ->
    handle_msg_close(Socket, State, ClientState);
handle_msg({udp_error, Socket, Reason}, {State, ClientState}) ->
    handle_msg_error(Socket, Reason, State, ClientState);
handle_msg(?MSG_CONNECT, {#state {
        client = Client,
        id = Id,
        init_options = Init,
        pool_name = PoolName,
        protocol = Protocol,
        reconnect_state = ReconnectState,
        socket = undefined
    } = State0, ClientState}) ->
    trace(State0, connecting, ok),
    case connect(State0) of
        {ok, #state{socket = Socket} = State} ->
            case client(Client, PoolName, Init, Protocol, Socket) of
                {ok, ClientState2} ->
                    ReconnectState2 = reconnect_state_reset(ReconnectState),
                    inc_metrics(State, shackle_connect_total),
                    shackle_status:enable(Id),
                    State1 = schedule_bounce(State#state{
                        reconnect_state = ReconnectState2,
                        timer_ref = undefined,
                        bounce_state = waiting
                    }),
                    ?ON_BOUNCE_EVENT(State1, #{
                        status => connected,
                        next_bounce => State1#state.next_bounce,
                        bounce_state => State1#state.bounce_state,
                        bounce_interval => State1#state.bounce_interval,
                        src => {?MODULE, ?LINE}
                    }),
                    inc_metrics(State, shackle_socket_total, <<"connect">>),
                    {ok, {State1, ClientState2}};
                {error, _Reason, ClientState2} ->
                    inc_metrics(State, shackle_error_total, <<"client connect error">>),
                    State1 = close_socket(State),
                    reconnect(State1, ClientState2)
            end;
        {error, _Reason} ->
            inc_metrics(State0, shackle_error_total, <<"socket connect error">>),
            reconnect(State0#state{socket = undefined}, ClientState)
    end;
handle_msg(?MSG_CONNECT, {#state{socket = Sock} = State, ClientState}) ->
    trace(State, connect_request_on_existing_socket, Sock),
    {ok, {State, ClientState}};
handle_msg({timeout, ExtRequestId}, {#state {
        client = Client,
        pool_name = PoolName
    } = State, ClientState}) ->

    inc_metrics(State, shackle_error_total, <<"timeout">>),
    case erlang:function_exported(Client, handle_timeout, 2) of
        true ->
            try Client:handle_timeout(ExtRequestId, ClientState) of
                {ok, Reply, ClientState2} ->
                    process_responses([Reply], State, ClientState2);
                {error, Reason, ClientState2} ->
                    inc_metrics(State, shackle_error_total, <<"handle_timeout error">>),
                    ?WARN(PoolName, "handle_timeout error: ~p", [Reason]),
                    close(State, ClientState2)
            catch
                ?EXCEPTION(E, R, Stacktrace) ->
                    inc_metrics(State, shackle_error_total, <<"handle_timeout exception">>),
                    ?WARN(PoolName, "handle_timeout error: ~p:~p~n  ~p~n",
                        [E, R, ?GET_STACK(Stacktrace)]),
                    close(State, ClientState)
            end;
        false ->
            case queue_take(ExtRequestId, State) of
                {Cast, State_} ->
                    reply({error, timeout}, [Cast], State_, ClientState);
                error ->
                    maybe_bounce(State, ClientState)
            end
    end;
handle_msg({bounce_connection, Sock}, {#state{socket = Sock} = State, ClientState}) ->
    bounce_check(State#state{bounce_timer_ref = undefined}, ClientState, false);
handle_msg({bounce_connection, Sock}, {State, ClientState}) ->
    trace(State, {bounce_connection, Sock}, wrong_socket),
    ?WARN(State#state.pool_name, "#~s bounce connection timer for wrong socket ~p",
        [State#state.srv_idx, Sock]),
    inc_metrics(State, shackle_error_total, <<"wrong socket bounce">>),
    %% Ignore the error
    {ok, {State, ClientState}};
handle_msg(bounce_connection, {#state{bounce_timer_ref = Ref} = State, ClientState}) ->
    %% Result of the bounce/1 API call
    cancel_timer(Ref),
    Now = now_time_ms(),
    bounce_check(State#state{bounce_timer_ref = undefined, next_bounce = Now}, ClientState, true);
handle_msg({queue_drain_check, S}, {#state{socket = S} = State, ClientState}) when S /= undefined ->
    maybe_bounce(State#state{drain_timer_ref = undefined}, ClientState);
handle_msg({queue_drain_check, Sock}, {State, ClientState}) ->
    trace(State, {queue_drain_check, Sock}, wrong_socket),
    ?WARN(State#state.pool_name, "queue drain check timer for wrong socket ~p", [Sock]),
    {ok, {State#state{drain_timer_ref = undefined}, ClientState}};
handle_msg({set_bounce_event, Fun}, {State, ClientState}) when is_function(Fun, 3); Fun==undefined ->
    {ok, {State#state{on_bounce_event = Fun}, ClientState}};
handle_msg({next_bounce, Pid, Ref}, {#state{bounce_interval = infinity}, _} = TupState) ->
    Pid ! {Ref, infinity},
    {ok, TupState};
handle_msg({next_bounce, Pid, Ref}, {#state{next_bounce = Time}, _} = TupState) ->
    Pid ! {Ref, Time - now_time_ms()},
    {ok, TupState};
handle_msg({probe, SentAt, _Bytes}, {#state{client = Client, pool_name = PoolName} = State, ClientState}) ->
    RecvAt = erlang:monotonic_time(),
    case persistent_term:get({shackle_probe_callback, Client}, undefined) of
        Fun when is_function(Fun, 2) ->
            DelayUs = erlang:convert_time_unit(RecvAt - SentAt, native, microsecond),
            Fun(PoolName, DelayUs);
        _ ->
            ok
    end,
    {ok, {State, ClientState}};
handle_msg(Msg, {#state{pool_name = PoolName} = State, ClientState}) ->
    ?WARN(PoolName, "unknown msg: ~p", [Msg]),
    {ok, {State, ClientState}}.

-spec terminate(term(), term()) ->
    ok.

terminate(Reason, {#state{client = Client, pool_name = PoolName} = State, ClientState}) ->
    ?WARN(PoolName, "terminate reason: ~p", [Reason]),
    close_socket(State),
    try Client:terminate(ClientState)
    catch
        ?EXCEPTION(E, R, Stacktrace) ->
            ?WARN(PoolName, "terminate crash: ~p:~p~n  ~p~n",
                [E, R, ?GET_STACK(Stacktrace)])
    end,
    State1 = State#state{socket = undefined, bounce_state = disconnected},
    reply_all({error, shutdown}, State1, undefined),
    ok.

%% private
ensure_loaded(M) ->
    case code:is_loaded(M) of
        false ->
            {module, M} == code:load_file(M)
                orelse error({cannot_load_module, M});
        _ ->
            ok
    end.

address(ClientOptions) ->
    case ?LOOKUP(address, ClientOptions) of
        undefined ->
            ?LOOKUP(ip, ClientOptions, ?DEFAULT_ADDRESS);
        Address ->
            Address
    end.

cancel_timer(#cast{timer_ref = Ref}) when is_reference(Ref) ->
    erlang:cancel_timer(Ref);
cancel_timer(Ref) when is_reference(Ref) ->
    erlang:cancel_timer(Ref);
cancel_timer(undefined) ->
    ok.

client(Client, PoolName, InitOptions, Protocol, Socket) ->
    case client_init(Client, PoolName, InitOptions) of
        {ok, ClientState} ->
            client_setup(Client, PoolName, Protocol, Socket, ClientState);
        {error, Reason} ->
            {error, Reason, undefined}
    end.

client_init(Client, PoolName, InitOptions) ->
    try client_init_(Client, PoolName, InitOptions) of
        {ok, ClientState} ->
            {ok, ClientState};
        {error, Reason} ->
            ?WARN(PoolName, "init error: ~p~n", [Reason]),
            {error, Reason}
    catch
        ?EXCEPTION(E, R, Stacktrace) ->
            ?WARN(PoolName, "init crash: ~p:~p~n  ~p~n",
                [E, R, ?GET_STACK(Stacktrace)]),
            {error, client_crash}
    end.

client_init_(Client, PoolName, InitOptions) ->
    case erlang:function_exported(Client, init, 2) of
        true ->
            Client:init(InitOptions, #{pool => PoolName});
        false ->
            case erlang:function_exported(Client, init, 1) of
                true ->
                    Client:init(InitOptions);
                false ->
                    {ok, undefined}
            end
    end.

client_setup(Client, PoolName, Protocol, Socket, ClientState) ->
    Protocol:setopts(Socket, [{active, false}]),
    try client_setup_(Client, Socket, ClientState) of
        {ok, ClientState2} ->
            Protocol:setopts(Socket, [{active, true}]),
            {ok, ClientState2};
        {error, Reason, ClientState2} ->
            ?WARN(PoolName, "setup error: ~p", [Reason]),
            {error, Reason, ClientState2}
    catch
        ?EXCEPTION(E, R, Stacktrace) ->
            ?WARN(PoolName, "setup error: ~p:~p~n  ~p~n",
                [E, R, ?GET_STACK(Stacktrace)]),
            {error, client_crash, ClientState}
    end.

client_setup_(Client, Socket, ClientState) ->
    case erlang:function_exported(Client, setup, 2) of
        true ->
            Client:setup(Socket, ClientState);
        false ->
            {ok, ClientState}
    end.

client_handle_request(Client, Cast, Request, ClientState) ->
    case Client:handle_request(Request, ClientState) of
        {ok, ExtRequestId, Data, ClientState_} ->
            {ok, Cast, ExtRequestId, Data, ClientState_};
        {ok, ExtRequestId, Data, RequestState, ClientState_} ->
            Cast_ = Cast#cast{request_state = RequestState},
            {ok, Cast_, ExtRequestId, Data, ClientState_};
        Else ->
            Else
    end.

client_handle_requests(Client, Casts, Requests, ClientState) ->
    case Client:handle_request(Requests, ClientState) of
        {ok, ExtRequestIds, Data, ClientState_} ->
            {ok, Casts, ExtRequestIds, Data, ClientState_};
        {ok, ExtRequestIds, Data, RequestStates, ClientState_} ->
            Casts_ = [Cast#cast{request_state = RequestState} ||
                {Cast, RequestState} <- lists:zip(Casts, RequestStates)],
            {ok, Casts_, ExtRequestIds, Data, ClientState_};
        Else ->
            Else
    end.

close(State, ClientState) ->
    close(State, ClientState, disconnected).
close(#state {id = Id} = State, ClientState, Reason)
    when Reason == disconnected; Reason == reconnecting ->
    shackle_status:disable(Id),
    ?ON_BOUNCE_EVENT(State, #{
        status => socket_close,
        bounce_state => Reason,
        bounce_interval => State#state.bounce_interval,
        src => {?MODULE, ?LINE}
    }),
    State1 = close_socket(State#state{bounce_state = Reason}),
    {ok, State2, ClientState2} =
        reply_all({error, socket_closed}, State1, ClientState),
    reconnect(State2, ClientState2).

close_socket(#state{socket = undefined} = State) ->
    State;
close_socket(#state{socket = Socket, protocol = Protocol,
                srv_idx = Idx, bounce_state = Reason
            } = State) ->
    Reason /= reconnecting andalso
        ?WARN(State#state.pool_name, "#~s: connection closed", [Idx]),
    Res = try Protocol:close(Socket)
          catch _:E -> {'EXIT', E}
          end,
    trace(State, close, Res),
    MetricsReason =
        case Reason of
            reconnecting -> <<"close reconnecting">>;
            _            -> <<"close">>
        end,
    inc_metrics(State, shackle_socket_total, MetricsReason),
    State#state{socket = undefined}.

connect(#state{
    servers = undefined,
    address = Address,
    srv_idx = ID,
    pool_name = PoolName,
    port = Port,
    protocol = Protocol,
    socket_options = SocketOptions
} = State) ->
    %% Single server mode (backwards compatibility)
    case inet:getaddrs(Address, inet) of
        {ok, Ips} when Ips /= [] ->
            Ip = shackle_utils:random_element(Ips),
            case Protocol:connect(Ip, Port, SocketOptions) of
                {ok, Socket} ->
                    inc_metrics(State, shackle_socket_total, <<"created">>),
                    State1 = State#state{socket = Socket, sock_id = State#state.sock_id+1},
                    trace(State1, connect, {ok, ?LINE}),
                    {ok, State1};
                {error, Reason} ->
                    ?WARN(PoolName, "#~s ~s:~w ~w connect error (~w): ~p", [ID, Address, Port, Protocol, Ip, Reason]),
                    trace(State#state{sock_id = State#state.sock_id+1}, connect_failed, {Reason, ?LINE}),
                    {error, Reason}
            end;
        {error, Reason} ->
            ?WARN(PoolName, "getaddrs ~p error: ~p", [Address, Reason]),
            trace(State#state{sock_id = State#state.sock_id+1}, getaddr_error, {Reason, ?LINE}),
            {error, Reason}
    end;
connect(#state{
    servers = ServersState,
    protocol = Protocol,
    socket_options = SocketOptions
} = State) when ServersState /= undefined ->
    %% Multi-server mode with failover
    try_servers(State, ServersState, Protocol, SocketOptions).

%% @private
%% Try to connect to servers in the list, one by one.
%% Returns {ok, UpdatedState} on success, or {error, Reason} after all servers fail.
try_servers(State, ServersState, Protocol, SocketOptions) ->
    try_servers_loop(State, ServersState, Protocol, SocketOptions).

%% @private
%% Iterate through servers using round-robin failover.
try_servers_loop(#state{
    srv_idx = ID,
    pool_name = PoolName
} = State, ServersState, Protocol, SocketOpts) ->
    %% Check if we've tried all servers
    case shackle_servers:all_failed(ServersState) of
        true ->
            %% All servers have been tried, fail
            {error, all_servers_failed};
        false ->
            %% Try to connect to the current server
            {Address, Port} = shackle_servers:current(ServersState),
            case inet:getaddrs(Address, inet) of
                {ok, Ips} when Ips /= [] ->
                    Ip = shackle_utils:random_element(Ips),
                    case Protocol:connect(Ip, Port, SocketOpts) of
                        {ok, Socket} ->
                            %% Connection successful
                            inc_metrics(State, shackle_socket_total, <<"created">>),
                            State1 = State#state{
                                socket = Socket,
                                sock_id = State#state.sock_id + 1,
                                servers = shackle_servers:reset_failed(ServersState)
                            },
                            trace(State1, connect, {ok, ?LINE}),
                            {ok, State1};
                        {error, Reason} ->
                            %% Connection failed, try next server
                            ?WARN(PoolName, "#~s ~s:~w ~w connect error (~w): ~p",
                                  [ID, Address, Port, Protocol, Ip, Reason]),
                            trace(State#state{sock_id = State#state.sock_id+1},
                                  connect_failed, {Reason, ?LINE}),
                            NewServersState = shackle_servers:next(ServersState),
                            try_servers_loop(State#state{servers = NewServersState},
                                           NewServersState, Protocol, SocketOpts)
                    end;
                {error, Reason} ->
                    %% DNS resolution failed, try next server
                    ?WARN(PoolName, "getaddrs ~p error: ~p", [Address, Reason]),
                    trace(State#state{sock_id = State#state.sock_id+1},
                          getaddr_error, {Reason, ?LINE}),
                    NewServersState = shackle_servers:next(ServersState),
                    try_servers_loop(State#state{servers = NewServersState},
                                   NewServersState, Protocol, SocketOpts)
            end
    end.

handle_msg_close(S, #state {socket = S} = State, ClientState) ->
    inc_metrics(State, shackle_close_total),
    trace(State, handle_msg_close, {S, ?LINE}),
    close(State, ClientState);

handle_msg_close(_Socket, State, ClientState) ->
    %% Ignore delayed socket close notifications for a non-owned or undefined socket
    {ok, {State, ClientState}}.

handle_msg_data(Socket, Data, #state {
        client = Client,
        pool_name = PoolName,
        socket = Socket
    } = State, ClientState) ->
    inc_metrics(State, shackle_received_bytes_total, byte_size(Data)),
    inc_metrics(State, shackle_received_messages_total, 1),

    try Client:handle_data(Data, ClientState) of
        {progress, ExtRequestId, Data2, ProcFun, ClientState_} ->
            case handle_progress(ExtRequestId, Data2, ProcFun, State, ClientState_) of
                {ok, ClientState2} ->
                    {ok, ClientState2};
                {error, Reason, ClientState2} ->
                    ?WARN(PoolName, "handle_progress error: ~p", [Reason]),
                    close(State, ClientState2)
            end;
        {ok, Replies, ClientState2} ->
            process_responses(Replies, State, ClientState2);
        {error, Reason, ClientState2} ->
            ?WARN(PoolName, "handle_data error: ~p", [Reason]),
            close(State, ClientState2)
    catch
        ?EXCEPTION(E, R, Stacktrace) ->
            ?WARN(PoolName, "handle_data crash: ~p:~p~n~p~n",
                [E, R, ?GET_STACK(Stacktrace)]),
            close(State, ClientState)
    end;
handle_msg_data(_Socket, _Data, State, ClientState) ->
    {ok, {State, ClientState}}.

handle_msg_error(Socket, Reason, #state {
        socket = Socket,
        pool_name = PoolName
    } = State, ClientState) ->

    inc_metrics(State, shackle_error_total, <<"socket error">>),
    ?WARN(PoolName, "connection error: ~p", [Reason]),
    close(State, ClientState).

handle_progress(ExtRequestId, Data, ProcFun, State, ClientState) ->
    case queue_find(ExtRequestId, State) of
        {ok, #cast{request_state = RequestState} = Cast} ->
            inc_metrics(State, shackle_response_total, <<"found">>),
            observe_metrics(State, Cast, shackle_response_time_microseconds),
            case ProcFun(Data, RequestState, ClientState) of
                {ok, Reply, ClientState_} ->
                    cancel_timer(Cast),
                    State_ = queue_remove(ExtRequestId, State),
                    reply(Reply, [Cast], State_, ClientState_);
                {continue, RequestState_, ClientState_} ->
                    Cast_ = Cast#cast{request_state = RequestState_},
                    State_ = queue_update(ExtRequestId, Cast_, State),
                    {ok, {State_, ClientState_}};
                {error, Reason, ClientState_} ->
                    {error, Reason, ClientState_}
            end;
        error ->
            inc_metrics(State, shackle_response_total, <<"not found">>),
            {ok, {State, ClientState}}
    end.

process_responses([], State, ClientState) ->
    {ok, {State, ClientState}};
process_responses([{ExtRequestId, Reply} | T], State, ClientState) ->
    case queue_take(ExtRequestId, State) of
        {Cast, State_} ->
            inc_metrics(State_, shackle_response_total, <<"found">>),
            observe_metrics(State_, Cast, shackle_response_time_microseconds),
            cancel_timer(Cast),
            {ok, {State2, CState2}} = reply(Reply, [Cast], State_, ClientState),
            process_responses(T, State2, CState2);
        error ->
            inc_metrics(State, shackle_response_total, <<"not found">>),
            process_responses(T, State, ClientState)
    end.

reconnect(State, undefined) ->
    reconnect_timer(State, undefined);
reconnect(#state {
        client = Client,
        pool_name = PoolName
    } = State, ClientState) ->

    try Client:terminate(ClientState)
    catch
        ?EXCEPTION(E, R, Stacktrace) ->
            ?WARN(PoolName, "terminate crash: ~p:~p~n~p~n",
                [E, R, ?GET_STACK(Stacktrace)])
    end,
    reconnect_timer(State, ClientState).

reconnect_state(Options) ->
    Reconnect = ?LOOKUP(reconnect, Options, ?DEFAULT_RECONNECT),
    case Reconnect of
        true ->
            Max = ?LOOKUP(reconnect_time_max, Options,
                ?DEFAULT_RECONNECT_MAX),
            Min = ?LOOKUP(reconnect_time_min, Options,
                ?DEFAULT_RECONNECT_MIN),

            #reconnect_state {
                min = Min,
                max = Max
            };
        false ->
            undefined
    end.

reconnect_state_reset(undefined) ->
    undefined;
reconnect_state_reset(#reconnect_state {} = ReconnectState) ->
    ReconnectState#reconnect_state {current = undefined}.

reconnect_timer(#state {bounce_state = reconnecting} = State0, ClientState) ->
    State = cancel_all_timers(State0),
    Ref = erlang:send_after(0, self(), ?MSG_CONNECT),
    State#state.socket /= undefined andalso
        ?LOG_WARNING("~w:~s reconnect_timer called with assigned socket",
            [State#state.pool_name, State#state.srv_idx]),
    State1 = State#state {timer_ref = Ref, bounce_state = disconnected},
    ?ON_BOUNCE_EVENT(State1, #{
        status => reconnect_timer,
        bounce_state => State#state.bounce_state,
        bounce_interval => 0,
        socket => State#state.socket,
        src => {?MODULE, ?LINE}
    }),
    {ok, {State1, ClientState}};

reconnect_timer(#state {reconnect_state = undefined} = State, ClientState) ->
    {ok, {State, ClientState}};

reconnect_timer(#state {reconnect_state = RState} = State0, ClientState)  ->
    State = cancel_all_timers(State0),
    RState2 = shackle_backoff:timeout(RState),
    Timeout = RState2#reconnect_state.current,
    Ref = erlang:send_after(Timeout, self(), ?MSG_CONNECT),
    State#state.socket /= undefined andalso
        ?LOG_WARNING("~w:~s reconnect_timer called with assigned socket",
            [State#state.pool_name, State#state.srv_idx]),
    ?ON_BOUNCE_EVENT(State, #{
        status => reconnect_timer,
        bounce_state => State#state.bounce_state,
        bounce_interval => Timeout,
        socket => State#state.socket,
        src => {?MODULE, ?LINE}
    }),
    {ok, {State#state {reconnect_state = RState2, timer_ref = Ref}, ClientState}}.

cancel_all_timers(State) ->
    cancel_timer(State#state.drain_timer_ref),
    cancel_timer(State#state.bounce_timer_ref),
    cancel_timer(State#state.timer_ref),
    State#state{
        drain_timer_ref = undefined,
        bounce_timer_ref = undefined,
        timer_ref = undefined
    }.

wrap(Casts) when is_list(Casts) ->
    Casts;
wrap(Cast) ->
    [Cast].

reply(Reply, Casts, State, ClientState) ->
    reply2(Reply, Casts, State),
    maybe_bounce(State, ClientState).

%% For batch replies Pid is the same in all casts in the list.
%% TODO: Optimize the batch replies to deliver them as 1 message to Pid?
reply2(_Reply, [], _State) ->
    ok;
reply2(Reply, [#cast {pid = Pid, send_reply = SendReply} = Cast | T], State) ->
    inc_metrics(State, shackle_reply_total),
    release(Cast),
    SendReply andalso Pid ! {Cast, Reply},
    reply2(Reply, T, State).

release(#cast {sema = Sema, pid = Pid}) ->
    shackle_sema:release(Sema, Pid).

reply_all(Reply, State, ClientState) ->
    {Requests, State_} = queue_clear(State),
    reply_all(Reply, Requests, State_, ClientState).

reply_all(_Reply, [], State, ClientState) ->
    {ok, State, ClientState};
reply_all(Reply, [Cast | T], State, ClientState) ->
    cancel_timer(Cast),
    {ok, {State1, ClientState1}} = reply(Reply, [Cast], State, ClientState),
    reply_all(Reply, T, State1, ClientState1).

handle_request_ids_from_client(ExtRequestIds, Casts, State, ClientState) ->
    ExtRequestIdsCasts = lists:zip(ExtRequestIds, Casts),
    lists:foldl(fun (IdCast, State_) ->
        handle_request_id_from_client(IdCast, State_, ClientState)
    end, State, ExtRequestIdsCasts).

handle_request_id_from_client({undefined, Cast}, State, ClientState) ->
    reply(ok, [Cast], State, ClientState),
    State;
handle_request_id_from_client({ExtRequestId, Cast}, State, _ClientState) ->
    set_receive_timeout(State, ExtRequestId, Cast).

set_receive_timeout(State, RequestId, #cast{timeout = Timeout} = Cast) ->
    Msg = {timeout, RequestId},
    TimerRef = erlang:send_after(Timeout, self(), Msg),
    queue_add(RequestId, Cast#cast{timer_ref = TimerRef}, State).

schedule_bounce(#state{bounce_interval = infinity} = State) ->
    State;
schedule_bounce(#state{bounce_interval = MSec, socket = Sock} = State) ->
    Time = now_time_ms() + MSec,
    Ref = erlang:send_after(MSec, self(), {bounce_connection, Sock}),
    State#state{next_bounce = Time, bounce_timer_ref = Ref}.

maybe_bounce(#state{bounce_state = BS, drain_timer_ref = OldTimerRef} = State, ClientState) when
    BS == draining; BS == awaiting_empty_queue
->
    %% If we are in the process of waiting for a connection bounce and draining
    %% the queue, when the queue has been drained, proceed with the bounce.
    case queue_empty(State) of
        true ->
            Pool = State#state.pool_name,
            shackle_pool:finalize_bounce(Pool),
            ?ON_BOUNCE_EVENT(State, #{
                status => finalizing_bounce,
                bounce_state => reconnecting,
                bounce_interval => State#state.bounce_interval,
                src => {?MODULE, ?LINE}
            }),
            inc_metrics(State, shackle_socket_total, <<"bounce">>),
            close(State#state{bounce_state = reconnecting}, ClientState, reconnecting);
        false ->
            State1 =
                if BS == draining, OldTimerRef == undefined ->
                    ?ON_BOUNCE_EVENT(State, #{
                        status => awaiting_empty_queue,
                        bounce_state => BS,
                        bounce_interval => State#state.bounce_interval,
                        queue_drain_timer => OldTimerRef,
                        src => {?MODULE, ?LINE}
                    }),
                    Ref = erlang:send_after(1000, self(), {queue_drain_check, State#state.socket}),
                    State#state{drain_timer_ref = Ref, bounce_state = awaiting_empty_queue};
                true ->
                    State
                end,
            {ok, {State1, ClientState}}
    end;
maybe_bounce(State, ClientState) ->
    {ok, {State, ClientState}}.

%% If bounce timeout is set and the current time exceeds the timeout,
%% enter the connection draining state
bounce_check(#state{bounce_interval = I, socket = Sock} = State, ClientState, _Force = false)
    when I == infinity; Sock == undefined ->
    {ok, {State, ClientState}};
bounce_check(#state{
        bounce_state = waiting,
        next_bounce = Time,
        socket = Sock,
        bounce_timer_ref = undefined
    } = State, ClientState, _)
->
    Interval = Time - now_time_ms(),
    case Interval =< 0 of
        true ->
            case shackle_pool:init_bounce(State#state.pool_name) of
                true ->
                    ?ON_BOUNCE_EVENT(State, #{
                        status => bounce_initiated,
                        bounce_state => draining,
                        server_status => disabled,
                        bounce_interval => State#state.bounce_interval,
                        src => {?MODULE, ?LINE}
                    }),
                    shackle_status:disable(State#state.id),
                    maybe_bounce(State#state{bounce_state = draining}, ClientState);
                false ->
                    %% Another connection is bouncing, wait for 1-4 seconds and
                    %% try bouncing again
                    NewInterval = 500 + rand:uniform(2000),
                    ?ON_BOUNCE_EVENT(State, #{
                        status => bounce_not_initiated,
                        reason => another_connection_bouncing,
                        next_bounce_check => NewInterval,
                        bounce_state => State#state.bounce_state,
                        bounce_interval => State#state.bounce_interval,
                        src => {?MODULE, ?LINE}
                    }),
                    Ref = erlang:send_after(NewInterval, self(), {bounce_connection, Sock}),
                    {ok, {State#state{bounce_timer_ref = Ref}, ClientState}}
            end;
        false ->
            ?ON_BOUNCE_EVENT(State, #{
                status => schedule_bounce_check,
                next_bounce_check => Interval,
                bounce_state => State#state.bounce_state,
                bounce_interval => State#state.bounce_interval,
                src => {?MODULE, ?LINE}
            }),
            Ref = erlang:send_after(Interval, self(), {bounce_connection, Sock}),
            {ok, {State#state{bounce_timer_ref = Ref}, ClientState}}
    end;
bounce_check(State, ClientState, _) ->
    {ok, {State, ClientState}}.

inc_metrics(State, Metric) ->
    inc_metrics(State, Metric, 1).
inc_metrics(#state{service_name = SName, pool_name = Pool}, Metric, Inc) when is_integer(Inc) ->
    log_metrics2(Metric, [SName, Pool], Inc);
inc_metrics(#state{service_name = SName, pool_name = Pool}, Metric, Reason) when is_binary(Reason) ->
    log_metrics2(Metric, [SName, Pool, Reason], 1).

log_metrics2(Metric, Args, N) ->
    prometheus_counter:inc(Metric, Args, N).

observe_metrics(#state{service_name = SName, pool_name = PoolName}, #cast{timestamp = Timestamp}, Metric) ->
    TimeDiff = os:system_time(microsecond) - Timestamp,
    prometheus_histogram:observe(Metric, [SName, PoolName], TimeDiff).

report_socket_rtt(#state{client = Client, pool_name = PoolName, socket = Socket}) ->
    case persistent_term:get({shackle_socket_rtt_callback, Client}, undefined) of
        Fun when is_function(Fun, 2) ->
            case get_socket_rtt(Socket) of
                {ok, RttUs} -> Fun(PoolName, RttUs);
                _ -> ok
            end;
        _ ->
            ok
    end.

-define(IPPROTO_TCP, 6).
-define(TCP_INFO, 11).
-define(TCP_INFO_SIZE, 256).
-define(RTT_OFFSET, 88).

get_socket_rtt(Socket) ->
    case inet:getopts(Socket, [{raw, ?IPPROTO_TCP, ?TCP_INFO, ?TCP_INFO_SIZE}]) of
        {ok, [{raw, ?IPPROTO_TCP, ?TCP_INFO, Bin}]} when byte_size(Bin) >= ?RTT_OFFSET + 4 ->
            <<_:?RTT_OFFSET/binary, RttUs:32/native-unsigned, _/binary>> = Bin,
            {ok, RttUs};
        _ ->
            error
    end.

now_time_ms() ->
    os:system_time(millisecond).

to_bin(V) when is_atom(V) -> atom_to_binary(V);
to_bin(V) when is_list(V) -> list_to_binary(V);
to_bin(V) when is_binary(V) -> V.

-ifdef(WITH_SHACKLE_TRACE_LOG).
init_trace_log() ->
    case ?GET_ENV(trace_log, true) of
        true ->
            DefName = lists:takewhile(fun(C) -> C /= $@ end, atom_to_list(node())),
            Path = ?GET_ENV(log_path, "/var/log/" ++ DefName),
            File = filename:join(filename:join(Path, "trace"), atom_to_list(Name) ++ ".log"),
            ?LOG_INFO("Using shackle trace file ~s", [File]),
            ok = filelib:ensure_dir(File),
            {ok, FD} = file:open(File, [append, raw, binary, {delayed_write, 4096, 500}]),
            FD;
        false ->
            undefined
    end.

trace(#state{trace_log = undefined}, _Event, _Args) ->
    ok;
trace(#state{socket = undefined}, _Event, _Args) ->
    ok;
trace(#state{trace_log = FD, socket = Sock, sock_id = ID}, Event, Args) ->
    TS = os:system_time(microsecond),
    {{Y,M,D},{HH,MM,SS}} = erlang:universaltime_to_localtime(erlang:posixtime_to_universaltime(TS div 1_000_000)),
    Endpoint = try shackle_utils:peername(Sock) catch _ -> "nil" end,
    Owner = try shackle_utils:sock_owner(Sock) catch _ -> "nil" end,
    EvID = case Event of connecting -> '-'; _ -> ID end,
    ok = file:write(FD, [
        i2b(Y), i2b(M), i2b(D), $-, i2b(HH), $:, i2b(MM), $:, i2b(SS), $.,
        io_lib:format("~.6.0w ~w ~p/~p ~p ~s ~w ~256p\n",
            [TS rem 1_000_000, EvID, self(), Owner, sock_info(Sock), Endpoint, Event, Args])
    ]).

sock_info(S) when is_port(S) -> S;
sock_info({_, _, {_Pid, {'$socket', Ref}}}) -> Ref;
sock_info(S) -> S.

i2b(I) when I < 9  -> <<$0, ($0+I)>>;
i2b(I) when I < 99 -> <<($0 + (I div 10)), ($0 + (I rem 10))>>;
i2b(I)             -> integer_to_binary(I).

-else.

init_trace_log() ->
    undefined.

trace(_, _, _) ->
    ok.
-endif.
