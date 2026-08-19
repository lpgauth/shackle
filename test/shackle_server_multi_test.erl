-module(shackle_server_multi_test).

-include("shackle_internal.hrl").
-include_lib("eunit/include/eunit.hrl").

%% Mock protocol for testing
-define(MOCK_PROTOCOL, shackle_server_multi_test_proto).

%%%=============================================================================
%% Setup/Teardown
%%%=============================================================================

setup_mock_protocol() ->
    Module = ?MOCK_PROTOCOL,
    code:load_file(Module),
    ok.

cleanup_mock_protocol(_) ->
    ok.

%%%=============================================================================
%% Tests for Multi-Server Configuration
%%%=============================================================================

%% @doc Test that multi-server configuration is properly initialized
multi_server_config_init_test_() ->
    {setup,
     fun setup_mock_protocol/0,
     fun cleanup_mock_protocol/1,
     [
         {<<"Single server in servers list">>,
          fun single_server_in_servers_test/0},
         {<<"Multiple servers normalization">>,
          fun multiple_servers_norm_test/0},
         {<<"Mixed server formats">>,
          fun mixed_servers_test/0}
     ]}.

single_server_in_servers_test() ->
    ServersList = ["127.0.0.1"],
    State = shackle_servers:new(ServersList, 9000),
    ?assertEqual(1, maps:get(total, State)),
    ?assertEqual([{"127.0.0.1", 9000}], maps:get(servers, State)).

multiple_servers_norm_test() ->
    ServersList = ["server1", "server2", "server3"],
    State = shackle_servers:new(ServersList, 9000),
    ?assertEqual(3, maps:get(total, State)),
    ?assertEqual([{"server1", 9000}, {"server2", 9000}, {"server3", 9000}],
                 maps:get(servers, State)).

mixed_servers_test() ->
    ServersList = ["server1", {"server2", 9001}, "server3"],
    State = shackle_servers:new(ServersList, 9000),
    ?assertEqual(3, maps:get(total, State)),
    ?assertEqual([{"server1", 9000}, {"server2", 9001}, {"server3", 9000}],
                 maps:get(servers, State)).

%%%=============================================================================
%% Tests for Failover Cycling
%%%=============================================================================

%% @doc Test round-robin cycling through servers
round_robin_test_() ->
    {setup,
     fun setup_mock_protocol/0,
     fun cleanup_mock_protocol/1,
     [
         {<<"First server current">>,
          fun first_server_current_test/0},
         {<<"Advance through all servers">>,
          fun advance_all_servers_test/0},
         {<<"Wrap around to first">>,
          fun wrap_around_test/0},
         {<<"Track failures">>,
          fun track_failures_test/0}
     ]}.

first_server_current_test() ->
    State = shackle_servers:new(["s1", "s2", "s3"], 9000),
    {Host, Port} = shackle_servers:current(State),
    ?assertEqual("s1", Host),
    ?assertEqual(9000, Port).

advance_all_servers_test() ->
    State0 = shackle_servers:new(["s1", "s2", "s3"], 9000),

    % First server
    {H1, _} = shackle_servers:current(State0),
    ?assertEqual("s1", H1),

    % Move to second
    State1 = shackle_servers:next(State0),
    {H2, _} = shackle_servers:current(State1),
    ?assertEqual("s2", H2),

    % Move to third
    State2 = shackle_servers:next(State1),
    {H3, _} = shackle_servers:current(State2),
    ?assertEqual("s3", H3).

wrap_around_test() ->
    State0 = shackle_servers:new(["s1", "s2", "s3"], 9000),
    State1 = shackle_servers:next(State0),
    State2 = shackle_servers:next(State1),
    State3 = shackle_servers:next(State2),

    % Should wrap back to first
    {H4, _} = shackle_servers:current(State3),
    ?assertEqual("s1", H4).

track_failures_test() ->
    State0 = shackle_servers:new(["s1", "s2", "s3"], 9000),
    ?assertEqual([], maps:get(failed, State0)),

    State1 = shackle_servers:next(State0),
    Failures1 = maps:get(failed, State1),
    ?assertEqual([0], Failures1),

    State2 = shackle_servers:next(State1),
    Failures2 = maps:get(failed, State2),
    ?assertEqual([1, 0], Failures2).

%%%=============================================================================
%% Tests for Failure Detection
%%%=============================================================================

%% @doc Test all_failed detection
failure_detection_test_() ->
    [
        {<<"Not all failed with one attempt">>,
         fun not_all_failed_one_test/0},
        {<<"Not all failed with partial attempts">>,
         fun not_all_failed_partial_test/0},
        {<<"All failed after full cycle">>,
         fun all_failed_full_cycle_test/0}
    ].

not_all_failed_one_test() ->
    State0 = shackle_servers:new(["s1", "s2", "s3"], 9000),
    ?assertNot(shackle_servers:all_failed(State0)).

not_all_failed_partial_test() ->
    State0 = shackle_servers:new(["s1", "s2", "s3"], 9000),
    State1 = shackle_servers:next(State0),
    ?assertNot(shackle_servers:all_failed(State1)).

all_failed_full_cycle_test() ->
    State0 = shackle_servers:new(["s1", "s2"], 9000),
    State1 = shackle_servers:next(State0),
    State2 = shackle_servers:next(State1),
    ?assert(shackle_servers:all_failed(State2)).

%%%=============================================================================
%% Tests for Reset Behavior
%%%=============================================================================

%% @doc Test resetting failed servers
reset_test_() ->
    [
        {<<"Reset clears failures">>,
         fun reset_clears_test/0},
        {<<"Reset keeps server list">>,
         fun reset_keeps_servers_test/0},
        {<<"Reset keeps current index">>,
         fun reset_keeps_index_test/0}
    ].

reset_clears_test() ->
    State0 = shackle_servers:new(["s1", "s2", "s3"], 9000),
    State1 = shackle_servers:next(State0),
    State2 = shackle_servers:next(State1),

    % Should have failures
    ?assert(length(maps:get(failed, State2)) > 0),

    % Reset
    State3 = shackle_servers:reset_failed(State2),

    % Should have no failures
    ?assertEqual([], maps:get(failed, State3)).

reset_keeps_servers_test() ->
    State0 = shackle_servers:new(["s1", "s2", "s3"], 9000),
    State1 = shackle_servers:next(State0),
    State2 = shackle_servers:next(State1),

    Servers1 = maps:get(servers, State2),
    State3 = shackle_servers:reset_failed(State2),
    Servers2 = maps:get(servers, State3),

    ?assertEqual(Servers1, Servers2).

reset_keeps_index_test() ->
    State0 = shackle_servers:new(["s1", "s2", "s3"], 9000),
    State1 = shackle_servers:next(State0),

    {H1, _} = shackle_servers:current(State1),

    State2 = shackle_servers:reset_failed(State1),
    {H2, _} = shackle_servers:current(State2),

    ?assertEqual(H1, H2).

%%%=============================================================================
%% Tests for Edge Cases
%%%=============================================================================

%% @doc Test edge cases and error conditions
edge_cases_test_() ->
    [
        {<<"Empty servers list error">>,
         fun empty_list_error_test/0},
        {<<"Single server doesn't fail too early">>,
         fun single_server_one_cycle_test/0},
        {<<"Atom hostnames">>,
         fun atom_hostnames_test/0}
    ].

empty_list_error_test() ->
    ?assertError({no_servers_configured, []}, shackle_servers:new([], 9000)).

single_server_one_cycle_test() ->
    State0 = shackle_servers:new(["s1"], 9000),
    ?assertNot(shackle_servers:all_failed(State0)),

    State1 = shackle_servers:next(State0),
    ?assert(shackle_servers:all_failed(State1)).

atom_hostnames_test() ->
    State = shackle_servers:new([server1, server2, server3], 9000),
    ?assertEqual(3, maps:get(total, State)),
    ?assertEqual([{server1, 9000}, {server2, 9000}, {server3, 9000}],
                 maps:get(servers, State)).

%%%=============================================================================
%% Tests for String Detection
%%%=============================================================================

%% @doc Test string vs list detection
string_detection_test_() ->
    [
        {<<"Single hostname string">>,
         fun single_string_test/0},
        {<<"List of hostnames">>,
         fun list_hostnames_test/0},
        {<<"Single character list">>,
         fun single_char_test/0},
        {<<"Empty list">>,
         fun empty_list_test/0}
    ].

single_string_test() ->
    State = shackle_servers:new("server.example.com", 9000),
    ?assertEqual(1, maps:get(total, State)),
    ?assertEqual([{"server.example.com", 9000}], maps:get(servers, State)).

list_hostnames_test() ->
    State = shackle_servers:new(["s1", "s2"], 9000),
    ?assertEqual(2, maps:get(total, State)).

single_char_test() ->
    % Single character should still be treated as a string/hostname
    State = shackle_servers:new("x", 9000),
    ?assertEqual(1, maps:get(total, State)),
    ?assertEqual([{"x", 9000}], maps:get(servers, State)).

empty_list_test() ->
    ?assertError({no_servers_configured, []}, shackle_servers:new([], 9000)).

%%%=============================================================================
%% Tests for Port Handling
%%%=============================================================================

%% @doc Test port configuration and overrides
port_handling_test_() ->
    [
        {<<"Default port applied">>,
         fun default_port_test/0},
        {<<"Per-server port override">>,
         fun per_server_port_test/0},
        {<<"Mixed ports">>,
         fun mixed_ports_test/0}
    ].

default_port_test() ->
    State = shackle_servers:new(["s1", "s2", "s3"], 8080),
    Servers = maps:get(servers, State),
    [_, {_, P2}, {_, P3}] = Servers,
    ?assertEqual(8080, P2),
    ?assertEqual(8080, P3).

per_server_port_test() ->
    State = shackle_servers:new([{"s1", 9001}], 8080),
    Servers = maps:get(servers, State),
    [{_, P}] = Servers,
    ?assertEqual(9001, P).

mixed_ports_test() ->
    State = shackle_servers:new([
        "s1",
        {"s2", 9001},
        "s3"
    ], 8080),
    Servers = maps:get(servers, State),
    [{_, P1}, {_, P2}, {_, P3}] = Servers,
    ?assertEqual(8080, P1),
    ?assertEqual(9001, P2),
    ?assertEqual(8080, P3).

%%%=============================================================================
%% Tests for Deterministic Round-Robin
%%%=============================================================================

%% @doc Test that round-robin is deterministic
determinism_test_() ->
    [
        {<<"Same sequence repeated">>,
         fun deterministic_sequence_test/0},
        {<<"Order preserved after reset">>,
         fun order_preserved_test/0}
    ].

deterministic_sequence_test() ->
    % First cycle
    S0a = shackle_servers:new(["a", "b", "c"], 9000),
    Seq1a = [element(1, shackle_servers:current(S0a))]
        ++ [element(1, shackle_servers:current(shackle_servers:next(S0a)))]
        ++ [element(1, shackle_servers:current(
                shackle_servers:next(shackle_servers:next(S0a))))],

    % Second cycle
    S0b = shackle_servers:new(["a", "b", "c"], 9000),
    Seq1b = [element(1, shackle_servers:current(S0b))]
        ++ [element(1, shackle_servers:current(shackle_servers:next(S0b)))]
        ++ [element(1, shackle_servers:current(
                shackle_servers:next(shackle_servers:next(S0b))))],

    ?assertEqual(Seq1a, Seq1b).

order_preserved_test() ->
    State0 = shackle_servers:new(["a", "b", "c"], 9000),
    State1 = shackle_servers:next(State0),
    State2 = shackle_servers:next(State1),
    State3 = shackle_servers:reset_failed(State2),

    % After reset, current index should still point to "c" (index 2)
    {H, _} = shackle_servers:current(State3),
    ?assertEqual("c", H).
