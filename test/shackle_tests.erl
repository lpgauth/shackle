-module(shackle_tests).
-include_lib("eunit/include/eunit.hrl").
-include("test.hrl").
-include("shackle_internal.hrl").

-define(N, 1000).

%% runners
shackle_app_stop_start_test_() ->
    {setup,
        fun () ->
            setup(),
            ?CLIENT_TCP:start()
        end,
        fun (_) -> cleanup(?CLIENT_TCP) end,
    [fun app_stop_start_subtest/0]}.

shackle_receive_response_timeout_test() ->
    %% receive_response/2 with a request id that will never get a reply
    %% must fall through the `after Timeout' branch and return
    %% {error, timeout}, regardless of shackle app state.
    FakeRequestId = {fake_pool_server, make_ref()},
    ?assertEqual({error, timeout},
        shackle:receive_response(FakeRequestId, 5)).

shackle_backlog_test_() ->
    {setup,
        fun () ->
            setup(?CLIENT_TCP, [
                {backlog_size, 1},
                {pool_size, 1}
            ])
        end,
        fun (_) -> cleanup(?CLIENT_TCP) end,
    [fun backlog_full_subtest/0]}.

shackle_backlog_infinity_test_() ->
    {setup,
        fun () ->
            setup(?CLIENT_TCP, [
                {backlog_size, infinity},
                {pool_size, 1}
            ])
        end,
        fun (_) -> cleanup(?CLIENT_TCP) end,
    [fun () -> add_subtest(?CLIENT_TCP) end]}.

shackle_call_crash_test_() ->
    {setup,
        fun () ->
            setup(?CLIENT_TCP, [
                {pool_size, 1}
            ])
        end,
        fun (_) -> cleanup(?CLIENT_TCP) end,
    [fun call_crash_subtest/0]}.

shackle_call_infinity_timeout_test_() ->
    {setup,
        fun () ->
            setup(?CLIENT_TCP, [
                {pool_size, 1}
            ])
        end,
        fun (_) -> cleanup(?CLIENT_TCP) end,
    [fun call_infinity_timeout_subtest/0]}.

shackle_call_no_server_test_() ->
    {setup,
        fun () ->
            setup(?CLIENT_TCP, [
                {pool_size, 1}
            ])
        end,
        fun (_) -> cleanup(?CLIENT_TCP) end,
    [fun call_no_server_subtest/0]}.

shackle_crash_disable_test_() ->
    {setup,
        fun () ->
            setup(?CLIENT_TCP, [
                {pool_size, 1}
            ])
        end,
        fun (_) -> cleanup(?CLIENT_TCP) end,
    [fun crash_disable_subtest/0]}.

shackle_mixed_timeout_test_() ->
    {setup,
        fun () ->
            setup(?CLIENT_TCP, [
                {pool_size, 1}
            ])
        end,
        fun (_) -> cleanup(?CLIENT_TCP) end,
    [fun mixed_timeout_subtest/0]}.

shackle_stale_sweep_test_() ->
    {setup,
        fun () ->
            setup(?CLIENT_TCP, [
                {pool_size, 1}
            ])
        end,
        fun (_) -> cleanup(?CLIENT_TCP) end,
    [fun stale_sweep_subtest/0]}.

shackle_stop_start_race_test_() ->
    {setup,
        fun () ->
            setup(?CLIENT_TCP, [
                {pool_size, 2}
            ])
        end,
        fun (_) -> cleanup(?CLIENT_TCP) end,
    [fun stop_start_race_subtest/0]}.

shackle_max_requests_test_() ->
    {setup,
        fun () ->
            setup(),
            arithmetic_tcp_server:start(),
            timer:sleep(100),
            shackle_pool:start(?POOL_NAME, ?CLIENT_TCP, [
                {max_requests, 20},
                {port, ?PORT},
                {reconnect, true},
                {reconnect_time_min, 1},
                {socket_options, [
                    binary,
                    {packet, raw}
                ]}
            ], [{pool_size, 1}]),
            timer:sleep(500)
        end,
        fun (_) -> cleanup(?CLIENT_TCP) end,
    [fun max_requests_subtest/0]}.

shackle_random_socket_test_() ->
    otp_28({setup,
        fun () ->
            setup(?CLIENT_SOCKET, [
                {pool_size, 1},
                {pool_strategy, random}
        ]) end,
        fun (_) -> cleanup(?CLIENT_SOCKET) end,
    {inparallel, [
        fun () -> add_subtest(?CLIENT_SOCKET) end,
        fun () -> multiply_subtest(?CLIENT_SOCKET) end,
        fun () -> noop_subtest(?CLIENT_SOCKET) end
    ]}}).

shackle_random_ssl_test_() ->
    {setup,
        fun () ->
            setup(?CLIENT_SSL, [
                {pool_size, 1},
                {pool_strategy, random}
        ]) end,
        fun (_) -> cleanup(?CLIENT_SSL) end,
    {inparallel, [
        fun () -> add_subtest(?CLIENT_SSL) end,
        fun () -> multiply_subtest(?CLIENT_SSL) end,
        fun () -> noop_subtest(?CLIENT_SSL) end
    ]}}.

shackle_random_ssl_socket_test_() ->
    otp_28({setup,
        fun () ->
            setup(?CLIENT_SSL_SOCKET, [
                {pool_size, 1},
                {pool_strategy, random}
        ]) end,
        fun (_) -> cleanup(?CLIENT_SSL_SOCKET) end,
    {inparallel, [
        fun () -> add_subtest(?CLIENT_SSL_SOCKET) end,
        fun () -> multiply_subtest(?CLIENT_SSL_SOCKET) end,
        fun () -> noop_subtest(?CLIENT_SSL_SOCKET) end
    ]}}).

shackle_random_tcp_test_() ->
    {setup,
        fun () ->
            setup(?CLIENT_TCP, [
                {pool_size, 1},
                {pool_strategy, random}
            ])
        end,
        fun (_) -> cleanup(?CLIENT_TCP) end,
    {inparallel, [
        fun () -> add_subtest(?CLIENT_TCP) end,
        fun () -> multiply_subtest(?CLIENT_TCP) end,
        fun () -> noop_subtest(?CLIENT_TCP) end
    ]}}.

shackle_random_udp_test_() ->
    {setup,
        fun () ->
            setup(?CLIENT_UDP, [
                {pool_size, 1},
                {pool_strategy, random}
            ])
        end,
        fun (_) -> cleanup(?CLIENT_UDP) end,
    {inparallel, [
        fun () -> add_subtest(?CLIENT_UDP) end,
        fun () -> multiply_subtest(?CLIENT_UDP) end,
        fun () -> noop_subtest(?CLIENT_UDP) end
    ]}}.

shackle_reconnect_socket_test_() ->
    otp_28({setup,
        fun () ->
            setup(),
            ?CLIENT_SOCKET:start()
        end,
        fun (_) -> cleanup(?CLIENT_SOCKET) end,
    [fun () -> reconnect_subtest(?CLIENT_SOCKET) end]}).

shackle_peer_close_socket_test_() ->
    otp_28({setup,
        fun () -> setup() end,
        fun (_) -> cleanup(?CLIENT_SOCKET) end,
    [fun peer_close_socket_subtest/0]}).

shackle_reconnect_ssl_test_() ->
    {setup,
        fun () ->
            setup(),
            ?CLIENT_SSL:start()
        end,
        fun (_) -> cleanup(?CLIENT_SSL) end,
    [fun () -> reconnect_subtest(?CLIENT_SSL) end]}.

shackle_reconnect_ssl_socket_test_() ->
    otp_28({setup,
        fun () ->
            setup(),
            ?CLIENT_SSL_SOCKET:start()
        end,
        fun (_) -> cleanup(?CLIENT_SSL_SOCKET) end,
    [fun () -> reconnect_subtest(?CLIENT_SSL_SOCKET) end]}).

shackle_reconnect_tcp_test_() ->
    {setup,
        fun () ->
            setup(),
            ?CLIENT_TCP:start()
        end,
        fun (_) -> cleanup(?CLIENT_TCP) end,
    [fun () -> reconnect_subtest(?CLIENT_TCP) end]}.

shackle_reconnect_udp_test_() ->
    {setup,
        fun () ->
            setup(),
            ?CLIENT_UDP:start()
        end,
        fun (_) -> cleanup(?CLIENT_UDP) end,
    [fun () -> reconnect_subtest(?CLIENT_UDP) end]}.

shackle_round_robin_tcp_test_() ->
    {setup,
        fun () ->
            setup(?CLIENT_TCP, [
                {pool_strategy, round_robin}
            ]) end,
        fun (_) -> cleanup(?CLIENT_TCP) end,
    {inparallel, [
        fun () -> add_subtest(?CLIENT_TCP) end,
        fun () -> multiply_subtest(?CLIENT_TCP) end,
        fun () -> noop_subtest(?CLIENT_TCP) end
    ]}}.

shackle_round_robin_udp_test_() ->
    {setup,
        fun () ->
            setup(?CLIENT_UDP, [
                {pool_strategy, round_robin}
            ])
        end,
        fun (_) -> cleanup(?CLIENT_UDP) end,
    {inparallel, [
        fun () -> add_subtest(?CLIENT_UDP) end,
        fun () -> multiply_subtest(?CLIENT_UDP) end,
        fun () -> noop_subtest(?CLIENT_UDP) end
    ]}}.

shackle_timeout_socket_test_() ->
    otp_28({setup,
        fun () ->
            setup(?CLIENT_SOCKET, [
                {pool_size, 1},
                {pool_strategy, random}
            ])
        end,
        fun (_) -> cleanup(?CLIENT_SOCKET) end,
    [fun () -> timeout_subtest(?CLIENT_SOCKET) end]}).

shackle_timeout_ssl_test_() ->
    {setup,
        fun () ->
            setup(?CLIENT_SSL, [
                {pool_size, 1},
                {pool_strategy, random}
            ])
        end,
        fun (_) -> cleanup(?CLIENT_SSL) end,
    [fun () -> timeout_subtest(?CLIENT_SSL) end]}.

shackle_timeout_ssl_socket_test_() ->
    otp_28({setup,
        fun () ->
            setup(?CLIENT_SSL_SOCKET, [
                {pool_size, 1},
                {pool_strategy, random}
            ])
        end,
        fun (_) -> cleanup(?CLIENT_SSL_SOCKET) end,
    [fun () -> timeout_subtest(?CLIENT_SSL_SOCKET) end]}).

shackle_timeout_tcp_test_() ->
    {setup,
        fun () ->
            setup(?CLIENT_TCP, [
                {pool_size, 1},
                {pool_strategy, random}
            ])
        end,
        fun (_) -> cleanup(?CLIENT_TCP) end,
    [fun () -> timeout_subtest(?CLIENT_TCP) end]}.

shackle_timeout_udp_test_() ->
    {setup,
        fun () ->
            setup(?CLIENT_UDP, [
                {pool_size, 1},
                {pool_strategy, random}
            ])
        end,
        fun (_) -> cleanup(?CLIENT_UDP) end,
    [fun () -> timeout_subtest(?CLIENT_UDP) end]}.

%% tests
add_subtest(Client) ->
    [assert_random_add(Client) || _ <- lists:seq(1, ?N)].

crash_disable_subtest() ->
    #pool_options {status = Status} =
        persistent_term:get({shackle_pool, ?POOL_NAME}),
    ?assertEqual(1, counters:get(Status, 1)),
    %% a crash while connected leaves the slot enabled; when the
    %% backend is down, the restarted server must disable it
    arithmetic_tcp_server:stop(),
    counters:put(Status, 1, 1),
    exit(whereis(server_name(1)), kill),
    timer:sleep(200),
    ?assertEqual(0, counters:get(Status, 1)),
    ?assertEqual({error, no_server}, arithmetic_tcp_client:add(1, 1)),
    ok = arithmetic_tcp_server:start().

app_stop_start_subtest() ->
    ?assertEqual({error, no_server}, arithmetic_tcp_client:add(1, 1)),
    ok = arithmetic_tcp_server:start(),
    timer:sleep(1000),
    ?assertEqual(2, arithmetic_tcp_client:add(1, 1)),
    ok = arithmetic_tcp_client:stop(),

    shackle_app:stop(),
    shackle_app:start(),

    arithmetic_tcp_client:start(),
    timer:sleep(1000),
    ?assertEqual(2, arithmetic_tcp_client:add(1, 1)).

backlog_full_subtest() ->
    Pid = self(),
    [spawn(fun () ->
        X = arithmetic_tcp_client:add(1, 1),
        Pid ! {response, X}
    end) || _ <- lists:seq(1, 20)],

    ?assert(lists:any(fun
        ({error, no_server}) -> true;
        (_) -> false
    end, receive_loop(20))).

call_infinity_timeout_subtest() ->
    ?assertEqual(3, shackle:call(?POOL_NAME, {add, 1, 2}, infinity)),
    ?assertEqual(5, shackle:call(?POOL_NAME, {add, 2, 3}, infinity)).

call_no_server_subtest() ->
    ServerName = server_name(1),
    ok = supervisor:terminate_child(shackle_sup, ServerName),
    ?assertEqual({error, no_server}, arithmetic_tcp_client:add(1, 1)),
    ?assertEqual({error, no_server},
        shackle:cast(?POOL_NAME, {add, 1, 1}, self(), ?TIMEOUT)),
    {ok, _} = supervisor:restart_child(shackle_sup, ServerName).

call_crash_subtest() ->
    ?assertEqual({error, client_crash}, arithmetic_tcp_client:add(a, b)),
    ?assertEqual(2, arithmetic_tcp_client:add(1, 1)).

max_requests_subtest() ->
    Socket1 = connected_socket(),
    [?assertEqual(2, arithmetic_tcp_client:add(1, 1)) ||
        _ <- lists:seq(1, 20)],
    timer:sleep(200),
    Socket2 = connected_socket(),
    ?assertNotEqual(Socket1, Socket2),
    ?assertEqual(2, arithmetic_tcp_client:add(1, 1)).

multiply_subtest(Client) ->
    [assert_random_multiply(Client) || _ <- lists:seq(1, ?N)].

noop_subtest(Client) ->
    [Client:noop() || _ <- lists:seq(1, 10)].

peer_close_socket_subtest() ->
    {ok, LSocket} = gen_tcp:listen(?PORT,
        [binary, {active, false}, {reuseaddr, true}]),
    Before = socket:number_of(),
    ok = ?CLIENT_SOCKET:start(),
    [gen_tcp:close(accept_init(LSocket)) || _ <- lists:seq(1, 10)],
    Socket = accept_init(LSocket),
    ?assertEqual(Before + 1, socket:number_of()),
    gen_tcp:close(Socket),
    gen_tcp:close(LSocket).

reconnect_subtest(Client) ->
    Server = server(Client),
    ?assertEqual({error, no_server}, Client:add(1, 1)),
    ok = Server:start(),
    timer:sleep(100),
    ?assertEqual(2, Client:add(1, 1)),
    ok = Server:stop(),
    timer:sleep(100),
    {error, _} = Client:add(1, 1),
    ok = Server:start(),
    timer:sleep(100),
    ?assertEqual(2, Client:add(1, 1)).

stale_sweep_subtest() ->
    Server = whereis(server_name(1)),
    {ok, RequestId} = shackle:cast(?POOL_NAME, {add, 255, 255}, self(), 300),
    Server ! {timeout, make_ref(), ?MSG_SWEEP},
    ?assertEqual({error, timeout}, shackle:receive_response(RequestId, 100)),
    ?assertEqual({error, timeout_handled},
        shackle:receive_response(RequestId, 1000)),
    ?assertEqual(Server, whereis(server_name(1))),
    timer:sleep(1000),
    ?assertEqual(2, arithmetic_tcp_client:add(1, 1)).

stop_start_race_subtest() ->
    Pid = self(),
    spawn(fun () -> Pid ! {stopped, shackle_pool:stop(?POOL_NAME)} end),
    ok = start_loop(),
    receive {stopped, ok} -> ok end,
    timer:sleep(500),
    ?assertEqual([true, true], [is_pid(whereis(server_name(N))) ||
        N <- [1, 2]]),
    ?assertEqual(2, arithmetic_tcp_client:add(1, 1)).

timeout_subtest(Client) ->
    ?assertEqual({error, timeout_handled}, Client:add(255, 255)).

%% utils
accept_init(LSocket) ->
    {ok, Socket} = gen_tcp:accept(LSocket, 1000),
    {ok, <<"INIT">>} = gen_tcp:recv(Socket, 0, 1000),
    ok = gen_tcp:send(Socket, <<"OK">>),
    Socket.

assert_random_add(Client) ->
    A = rand(),
    B = rand(),
    ?assertEqual(A + B, Client:add(A, B)).

assert_random_multiply(Client) ->
    A = rand(),
    B = rand(),
    ?assertEqual(A * B, Client:multiply(A, B)).

cleanup() ->
    shackle_app:stop().

cleanup(Client) ->
    Client:stop(),
    Server = server(Client),
    Server:stop(),
    cleanup().

connected_socket() ->
    ServerName = server_name(1),
    {links, Links} = erlang:process_info(whereis(ServerName), links),
    [Socket] = [Port || Port <- Links, is_port(Port)],
    Socket.

mixed_timeout_subtest() ->
    %% the second request has the earlier deadline, so the server
    %% cancels the sweep timer and starts it again
    {ok, _} = shackle:cast(?POOL_NAME, {add, 255, 255}, undefined, 5000),
    {Microseconds, Reply} = timer:tc(fun () ->
        shackle:call(?POOL_NAME, {add, 255, 255}, 100)
    end),
    ?assertEqual({error, timeout_handled}, Reply),
    ?assert(Microseconds < 1000000).

start_loop() ->
    case arithmetic_tcp_client:start([{pool_size, 2}]) of
        {error, pool_already_started} ->
            start_loop();
        Result ->
            Result
    end.

otp_28(Tests) ->
    case list_to_integer(erlang:system_info(otp_release)) >= 28 of
        true ->
            Tests;
        false ->
            []
    end.

rand() ->
    shackle_utils:random(254).

receive_loop(0) ->
    [];
receive_loop(N) ->
    receive
        {response, X} ->
            [X | receive_loop(N - 1)]
    end.

server(?CLIENT_SOCKET) ->
    arithmetic_tcp_server;
server(?CLIENT_SSL) ->
    arithmetic_ssl_server;
server(?CLIENT_SSL_SOCKET) ->
    arithmetic_ssl_server;
server(?CLIENT_TCP) ->
    arithmetic_tcp_server;
server(?CLIENT_UDP) ->
    arithmetic_udp_server.

server_name(Index) ->
    list_to_atom(atom_to_list(?POOL_NAME) ++ "_" ++ integer_to_list(Index)).

setup() ->
    error_logger:tty(false),
    shackle_app:start().

setup(Client, Options) ->
    setup(),
    Server = server(Client),
    Server:start(),
    timer:sleep(100),
    Client:start(Options),
    timer:sleep(500).
