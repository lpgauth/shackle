-module(shackle_tests).
-include_lib("eunit/include/eunit.hrl").
-include("test.hrl").

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
        fun () -> multiply_subtest(?CLIENT_SSL) end
    ]}}.

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
        fun () -> multiply_subtest(?CLIENT_TCP) end
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
        fun () -> multiply_subtest(?CLIENT_UDP) end
    ]}}.

shackle_reconnect_ssl_test_() ->
    {setup,
        fun () ->
            setup(),
            ?CLIENT_SSL:start()
        end,
        fun (_) -> cleanup(?CLIENT_SSL) end,
    [fun () -> reconnect_subtest(?CLIENT_SSL) end]}.

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
        fun () -> multiply_subtest(?CLIENT_TCP) end
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
        fun () -> multiply_subtest(?CLIENT_UDP) end
    ]}}.

%% tests
add_subtest(Client) ->
    [assert_random_add(Client) || _ <- lists:seq(1, ?N)].

app_stop_start_subtest() ->
    ?assertEqual({error, no_socket}, arithmetic_tcp_client:add(1, 1)),
    ok = arithmetic_tcp_server:start(),
    timer:sleep(100),
    ?assertEqual(2, arithmetic_tcp_client:add(1, 1)),

    shackle_app:stop(),
    timer:sleep(100),
    shackle_app:start(),

    arithmetic_tcp_client:start(),
    timer:sleep(100),
    ?assertEqual(2, arithmetic_tcp_client:add(1, 1)).

backlog_full_subtest() ->
    Pid = self(),
    [spawn(fun () ->
        X = arithmetic_tcp_client:add(1, 1),
        Pid ! {response, X}
    end) || _ <- lists:seq(1, 20)],

    ?assert(lists:any(fun
        ({error, backlog_full}) -> true;
        (_) -> false
    end, receive_loop(20))).

call_crash_subtest() ->
    ?assertEqual({error, client_crash}, arithmetic_tcp_client:add(a, b)),
    ?assertEqual(2, arithmetic_tcp_client:add(1, 1)).

multiply_subtest(Client) ->
    [assert_random_multiply(Client) || _ <- lists:seq(1, ?N)].

reconnect_subtest(Client) ->
    Server = server(Client),
    ?assertEqual({error, no_socket}, Client:add(1, 1)),
    ok = Server:start(),
    timer:sleep(100),
    ?assertEqual(2, Client:add(1, 1)),
    ok = Server:stop(),
    {error, _} = Client:add(1, 1), % no_socket (ssl, tcp) or timeout (udp)
    ok = Server:start(),
    timer:sleep(100),
    ?assertEqual(2, Client:add(1, 1)).

%% utils
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

rand() ->
    shackle_utils:random(255).

receive_loop(0) ->
    [];
receive_loop(N) ->
    receive
        {response, X} ->
            [X | receive_loop(N - 1)]
    end.

server(?CLIENT_SSL) ->
    arithmetic_ssl_server;
server(?CLIENT_TCP) ->
    arithmetic_tcp_server;
server(?CLIENT_UDP) ->
    arithmetic_udp_server.

setup() ->
    error_logger:tty(false),
    shackle_app:start().

setup(Client, Options) ->
    setup(),
    Server = server(Client),
    Server:start(),
    timer:sleep(100),
    Client:start(Options).
