-module(shackle_servers_test).

-include_lib("eunit/include/eunit.hrl").

%%%=============================================================================
%% Tests
%%%=============================================================================

%% @doc Test normalization of servers list with bare hostnames.
servers_normalization_bare_hostnames_test() ->
    ServersList = ["server1.example.com", "server2.example.com", "server3.example.com"],
    State = shackle_servers:new(ServersList, 9000),
    ?assertEqual(3, maps:get(total, State)),
    ?assertEqual([
        {"server1.example.com", 9000},
        {"server2.example.com", 9000},
        {"server3.example.com", 9000}
    ], maps:get(servers, State)).

%% @doc Test normalization of servers list with per-server ports.
servers_normalization_per_server_ports_test() ->
    ServersList = [
        {"server1.example.com", 9000},
        {"server2.example.com", 9001},
        {"server3.example.com", 9002}
    ],
    State = shackle_servers:new(ServersList, 8000),
    ?assertEqual(3, maps:get(total, State)),
    ?assertEqual([
        {"server1.example.com", 9000},
        {"server2.example.com", 9001},
        {"server3.example.com", 9002}
    ], maps:get(servers, State)).

%% @doc Test normalization of mixed servers list.
servers_normalization_mixed_test() ->
    ServersList = [
        "server1.example.com",
        {"server2.example.com", 9001},
        "server3.example.com"
    ],
    State = shackle_servers:new(ServersList, 9000),
    ?assertEqual(3, maps:get(total, State)),
    ?assertEqual([
        {"server1.example.com", 9000},
        {"server2.example.com", 9001},
        {"server3.example.com", 9000}
    ], maps:get(servers, State)).

%% @doc Test single server configuration.
single_server_config_test() ->
    State = shackle_servers:new("server1.example.com", 9000),
    ?assertEqual(1, maps:get(total, State)),
    ?assertEqual([{"server1.example.com", 9000}], maps:get(servers, State)).

%% @doc Test getting current server address.
current_server_test() ->
    ServersList = ["server1", "server2", "server3"],
    State = shackle_servers:new(ServersList, 9000),
    ?assertEqual({"server1", 9000}, shackle_servers:current(State)).

%% @doc Test moving to next server.
next_server_test() ->
    ServersList = ["server1", "server2", "server3"],
    State = shackle_servers:new(ServersList, 9000),
    State1 = shackle_servers:next(State),
    ?assertEqual({"server2", 9000}, shackle_servers:current(State1)),
    State2 = shackle_servers:next(State1),
    ?assertEqual({"server3", 9000}, shackle_servers:current(State2)),
    State3 = shackle_servers:next(State2),
    %% Should wrap around to first server
    ?assertEqual({"server1", 9000}, shackle_servers:current(State3)).

%% @doc Test round-robin cycling through all servers.
round_robin_cycling_test() ->
    ServersList = ["server1", "server2", "server3"],
    State0 = shackle_servers:new(ServersList, 9000),

    %% Cycle through all servers
    State1 = shackle_servers:next(State0),
    State2 = shackle_servers:next(State1),
    State3 = shackle_servers:next(State2),

    %% Now all servers should be marked as failed
    ?assert(shackle_servers:all_failed(State3)).

%% @doc Test resetting failed servers after successful connection.
reset_failed_test() ->
    ServersList = ["server1", "server2", "server3"],
    State0 = shackle_servers:new(ServersList, 9000),

    %% Try some servers
    State1 = shackle_servers:next(State0),
    State2 = shackle_servers:next(State1),

    %% Should have failures
    Failures = maps:get(failed, State2),
    ?assert(length(Failures) > 0),

    %% Reset failures
    State3 = shackle_servers:reset_failed(State2),
    ?assertEqual([], maps:get(failed, State3)).

%% @doc Test all_failed returns false when not all servers tried.
all_failed_false_test() ->
    ServersList = ["server1", "server2", "server3"],
    State0 = shackle_servers:new(ServersList, 9000),
    State1 = shackle_servers:next(State0),

    %% Should not be all failed yet (only tried 2 of 3)
    ?assertNot(shackle_servers:all_failed(State1)).

%% @doc Test all_failed returns true after trying all servers.
all_failed_true_test() ->
    ServersList = ["server1", "server2"],
    State0 = shackle_servers:new(ServersList, 9000),
    State1 = shackle_servers:next(State0),
    State2 = shackle_servers:next(State1),

    %% Should be all failed (tried 2 of 2)
    ?assert(shackle_servers:all_failed(State2)).

%% @doc Test empty servers list raises error.
empty_servers_list_error_test() ->
    ?assertError({no_servers_configured, []}, shackle_servers:new([], 9000)).

%% @doc Test atom as server address.
atom_server_address_test() ->
    ServersList = [server1, server2, server3],
    State = shackle_servers:new(ServersList, 9000),
    ?assertEqual(3, maps:get(total, State)),
    ?assertEqual([
        {server1, 9000},
        {server2, 9000},
        {server3, 9000}
    ], maps:get(servers, State)).

%%%=============================================================================
%% DNS Resolution Tests
%%%=============================================================================

%% @doc Test DNS resolution with IPv4 string.
resolve_host_ipv4_string_test() ->
    {ok, [IP]} = shackle_servers:resolve_host("127.0.0.1"),
    ?assertEqual({127, 0, 0, 1}, IP).

%% @doc Test DNS resolution with IPv4 tuple.
resolve_host_ipv4_tuple_test() ->
    {ok, [IP]} = shackle_servers:resolve_host({127, 0, 0, 1}),
    ?assertEqual({127, 0, 0, 1}, IP).

%% @doc Test DNS resolution with IPv6 tuple.
resolve_host_ipv6_tuple_test() ->
    IPv6 = {0, 0, 0, 0, 0, 0, 0, 1},
    {ok, [IP]} = shackle_servers:resolve_host(IPv6),
    ?assertEqual(IPv6, IP).

%% @doc Test DNS resolution with atom hostname.
resolve_host_atom_test() ->
    {ok, IPs} = shackle_servers:resolve_host(localhost),
    ?assert(length(IPs) > 0),
    [IP | _] = IPs,
    ?assertMatch({_,_,_,_}, IP).

%% @doc Test DNS resolution with localhost string.
resolve_host_localhost_test() ->
    {ok, Addrs} = shackle_servers:resolve_host("localhost"),
    ?assert(length(Addrs) > 0).

%% @doc Test DNS resolution with invalid format.
resolve_host_invalid_test() ->
    ?assertMatch({error, _}, shackle_servers:resolve_host(invalid_atom_not_ip)).

%% @doc Test DNS caching by verifying same result on repeated calls.
resolve_host_caching_test() ->
    % Clear cache first
    erase({dns_cache, "localhost"}),
    {ok, Addrs1} = shackle_servers:resolve_host("localhost"),
    {ok, Addrs2} = shackle_servers:resolve_host("localhost"),
    ?assertEqual(Addrs1, Addrs2).

%%%=============================================================================
%% Kubernetes Service Detection Tests
%%%=============================================================================

%% @doc Test K8s service name detection
is_k8s_service_name_test() ->
    % K8s service names contain ".svc." or ".cluster.local"
    ?assert(shackle_servers:is_k8s_service_name("my-service.svc.cluster.local")),
    ?assert(shackle_servers:is_k8s_service_name("my-service.default.svc.default")),
    ?assert(shackle_servers:is_k8s_service_name("my-service.cluster.local")),
    ?assertNot(shackle_servers:is_k8s_service_name("example.com")),
    ?assertNot(shackle_servers:is_k8s_service_name("localhost")).

%% @doc Test fully qualified K8s service name detection
is_fully_qualified_k8s_name_test() ->
    % Fully qualified K8s names contain ".svc."
    ?assert(shackle_servers:is_fully_qualified_k8s_name("my-service.svc.cluster.local")),
    ?assert(shackle_servers:is_fully_qualified_k8s_name("my-service.default.svc.cluster.local")),
    ?assertNot(shackle_servers:is_fully_qualified_k8s_name("my-service")),
    ?assertNot(shackle_servers:is_fully_qualified_k8s_name("example.com")).

%% @doc Test K8s environment detection
is_k8s_environment_test() ->
    % Should detect K8s environment (or not, depending on runtime)
    Result = shackle_servers:is_k8s_environment(),
    ?assert(is_boolean(Result)).

%% @doc Test getting cluster domain
get_cluster_domain_test() ->
    Domain = shackle_servers:get_cluster_domain(),
    ?assert(is_list(Domain)),
    ?assert(length(Domain) > 0).

%% @doc Test getting current namespace
get_current_namespace_test() ->
    % Test with external environment
    Namespace = shackle_servers:get_current_namespace(external),
    ?assertEqual("default", Namespace),

    % Test with in-cluster environment
    Namespace2 = shackle_servers:get_current_namespace({in_cluster, "myns"}),
    ?assertEqual("myns", Namespace2),

    % Test with external K8s env
    Namespace3 = shackle_servers:get_current_namespace({external_with_k8s_env, "prodns"}),
    ?assertEqual("prodns", Namespace3).

%% @doc Test building K8s search domains
build_k8s_search_domains_test() ->
    Domains = shackle_servers:build_k8s_search_domains("my-service", external),
    ?assert(is_list(Domains)),
    ?assert(length(Domains) >= 1),

    % Domains should contain cluster.local reference
    ?assert(lists:any(fun(D) -> string:str(D, "cluster.local") > 0 end, Domains)).

%% @doc Test detecting K8s environment
detect_k8s_environment_test() ->
    Env = shackle_servers:detect_k8s_environment(),
    % Should be one of: external, {in_cluster, Namespace}, {external_with_k8s_env, Namespace}
    case Env of
        external -> ?assert(true);
        {in_cluster, NS} -> ?assert(is_list(NS));
        {external_with_k8s_env, NS} -> ?assert(is_list(NS));
        _ -> ?assertMatch(external, Env)  % Fallback - should match external
    end.
