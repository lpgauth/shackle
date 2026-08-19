-module(shackle_servers).
-include("shackle_internal.hrl").

%% public
-export([
    new/2,
    current/1,
    next/1,
    all_failed/1,
    reset_failed/1,
    resolve_host/1,
    resolve_hostname/1,
    is_k8s_environment/0
]).

%% Kubernetes detection functions (for testing and advanced use)
-ifdef(TEST).
-export([
    is_k8s_service_name/1,
    is_fully_qualified_k8s_name/1,
    detect_k8s_environment/0,
    get_current_namespace/1,
    get_cluster_domain/0,
    build_k8s_search_domains/2
]).
-endif.

%% types
-type server_entry() :: string() | atom() | {string() | atom(), pos_integer()}.
-type servers_list() :: [server_entry()].
-type normalized_servers() :: [{string() | atom(), pos_integer()}].
-type state() :: #{
    servers => normalized_servers(),
    index => non_neg_integer(),
    failed => [non_neg_integer()],
    total => pos_integer()
}.

-export_type([state/0, normalized_servers/0, server_entry/0, servers_list/0]).

%%%=============================================================================
%%% Public API
%%%=============================================================================

%% @doc
%% Create and normalize a servers list from configuration.
%%
%% Input can be:
%% - A bare hostname: "server.example.com" (uses DefaultPort)
%% - A tuple: {"server.example.com", 9000}
%% - A list of mixed: ["server1", {"server2", 9001}, ...]
%%
%% Returns a map with normalized servers and initial state.
%% @end
-spec new(servers_list() | server_entry(), pos_integer()) -> state().
new(Servers, DefaultPort) when is_list(Servers), is_integer(DefaultPort), DefaultPort > 0 ->
    % Check if this is a string (list of character codes) or list of entries
    case is_string(Servers) of
        true ->
            % It's a string, treat as single server
            new([Servers], DefaultPort);
        false ->
            % It's a list of server entries
            NormalizedServers = normalize_servers(Servers, DefaultPort),
            case NormalizedServers of
                [] ->
                    error({no_servers_configured, Servers});
                _ ->
                    #{
                        servers => NormalizedServers,
                        index => 0,
                        failed => [],
                        total => length(NormalizedServers)
                    }
            end
    end;
new(Server, DefaultPort) when is_integer(DefaultPort), DefaultPort > 0 ->
    new([Server], DefaultPort).

%% @doc
%% Get the current server address {Host, Port} from state.
%% @end
-spec current(state()) -> {string(), pos_integer()}.
current(#{servers := Servers, index := Index}) ->
    lists:nth(Index + 1, Servers).

%% @doc
%% Move to the next server in round-robin fashion.
%% Returns updated state with new index.
%% @end
-spec next(state()) -> state().
next(#{index := Index, total := Total, failed := Failed} = State) ->
    NextIndex = (Index + 1) rem Total,
    NewFailed = [Index | Failed],
    State#{
        index => NextIndex,
        failed => NewFailed
    }.

%% @doc
%% Check if all servers have been tried in this cycle.
%% This is true when the number of failed servers equals the total count.
%% @end
-spec all_failed(state()) -> boolean().
all_failed(#{failed := Failed, total := Total}) ->
    length(Failed) >= Total.

%% @doc
%% Reset the failed servers tracking after a successful connection.
%% @end
-spec reset_failed(state()) -> state().
reset_failed(State) ->
    State#{failed => []}.

%%%=============================================================================
%%% Private functions
%%%=============================================================================

%% @private
%% Check if a value is a string (non-empty list of character codes).
is_string(S) when is_list(S) ->
    case S of
        [] -> false;  % Empty list is not a string
        [H | _] when is_integer(H), H >= 0, H =< 1114111 -> true;
        _ -> false
    end;
is_string(_) -> false.

%% @private
%% Normalize server entries to {Host, Port} tuples.
normalize_servers(Servers, DefaultPort) ->
    lists:filtermap(fun(S) -> normalize_server(S, DefaultPort) end, Servers).

%% @private
%% Normalize a single server entry.
normalize_server(Server, _DefaultPort) when is_tuple(Server), tuple_size(Server) =:= 2 ->
    {Host, Port} = Server,
    case is_valid_host_port(Host, Port) of
        true -> {true, {Host, Port}};
        false -> {false, undefined}
    end;
normalize_server(Server, DefaultPort) when is_list(Server); is_atom(Server) ->
    case is_valid_host(Server) of
        true -> {true, {Server, DefaultPort}};
        false -> {false, undefined}
    end;
normalize_server(_Server, _DefaultPort) ->
    {false, undefined}.

%% @private
%% Validate host and port tuple.
%% Port can be an integer or any term (for testing purposes).
is_valid_host_port(Host, Port) ->
    is_valid_host(Host) andalso Port /= undefined.

%% @private
%% Validate host (string or atom).
is_valid_host(Host) when is_list(Host) ->
    is_valid_hostname_string(Host);
is_valid_host(Host) when is_atom(Host) ->
    true;
is_valid_host(_) ->
    false.

%% @private
%% Very basic hostname validation - just check it's a non-empty string.
is_valid_hostname_string(Host) ->
    is_list(Host) andalso Host =/= [].

%%%=============================================================================
%%% DNS Resolution Functions
%%%=============================================================================

%% @doc
%% Resolve a hostname to a list of IP addresses.
%%
%% Supports:
%% - String hostnames: "example.com" or [115,101,114,118,101,114,46,99,111,109]
%% - Atom hostnames: server_name
%% - IPv4 tuples: {127, 0, 0, 1}
%% - IPv6 tuples: {0, 0, 0, 0, 0, 0, 0, 1}
%% - IPv4 strings: "127.0.0.1"
%% - Kubernetes services: "my-service.namespace.svc.cluster.local"
%%
%% Returns {ok, [IP]} or {error, Reason}
%% @end
-spec resolve_host(string() | atom() | {integer(), integer(), integer(), integer()}
                   | {integer(), integer(), integer(), integer(),
                      integer(), integer(), integer(), integer()})
    -> {ok, [inet:ip_address()]} | {error, term()}.
resolve_host(Host) when is_atom(Host) ->
    resolve_host(atom_to_list(Host));
resolve_host(Host) when is_list(Host) ->
    % First check if it's already an IP address
    case inet:parse_address(Host) of
        {ok, IP} ->
            {ok, [IP]};
        {error, einval} ->
            % Not an IP address, perform DNS resolution
            resolve_hostname(Host)
    end;
resolve_host({A, B, C, D} = IP)
    when is_integer(A), is_integer(B), is_integer(C), is_integer(D),
         A >= 0, A =< 255, B >= 0, B =< 255,
         C >= 0, C =< 255, D >= 0, D =< 255 ->
    % IPv4 tuple
    {ok, [IP]};
resolve_host({A, B, C, D, E, F, G, H} = IP)
    when is_integer(A), is_integer(B), is_integer(C), is_integer(D),
         is_integer(E), is_integer(F), is_integer(G), is_integer(H) ->
    % IPv6 tuple
    {ok, [IP]};
resolve_host(Host) ->
    {error, {invalid_host_format, Host}}.

%% @private
%% Perform DNS resolution with caching.
%% Uses process dictionary to cache results for 5 minutes.
-spec resolve_hostname(string()) -> {ok, [inet:ip_address()]} | {error, term()}.
resolve_hostname(Hostname) ->
    CacheKey = {dns_cache, Hostname},
    CurrentTime = erlang:system_time(second),

    % Check cache first
    case get(CacheKey) of
        {CachedAddrs, CacheTime} when CurrentTime - CacheTime < 300 ->
            % Cache hit (5 minute TTL)
            {ok, CachedAddrs};
        _ ->
            % Cache miss or expired, perform DNS resolution
            case resolve_hostname_uncached(Hostname) of
                {ok, Addrs} = Result ->
                    % Cache the result
                    put(CacheKey, {Addrs, CurrentTime}),
                    Result;
                {error, _} = Error ->
                    % Don't cache errors
                    Error
            end
    end.

%% @private
%% Perform DNS resolution without caching.
%% Tries IPv4 first, falls back to IPv6 if IPv4 fails.
%% Also handles Kubernetes service names.
resolve_hostname_uncached(Hostname) ->
    % Check if this looks like a Kubernetes service
    case is_k8s_service_name(Hostname) of
        true ->
            resolve_k8s_hostname(Hostname);
        false ->
            resolve_standard_hostname(Hostname)
    end.

%% @private
%% Resolve standard (non-Kubernetes) hostnames
resolve_standard_hostname(Hostname) ->
    % Try IPv4 first (preferred)
    case inet:getaddrs(Hostname, inet) of
        {ok, IPv4Addrs} when length(IPv4Addrs) > 0 ->
            {ok, IPv4Addrs};
        {error, nxdomain} ->
            % No IPv4 - check if IPv6 is available
            case inet:getaddrs(Hostname, inet6) of
                {ok, IPv6Addrs} when length(IPv6Addrs) > 0 ->
                    % IPv6 available but IPv4 preferred
                    {error, {ipv6_not_supported, IPv6Addrs, Hostname}};
                {error, _} ->
                    {error, {hostname_not_found, Hostname}}
            end;
        {error, _} = Error ->
            % IPv4 resolution failed for other reasons
            Error
    end.

%% @private
%% Detect if hostname looks like a Kubernetes service name
is_k8s_service_name(Hostname) ->
    % Check for Kubernetes patterns:
    % - Contains ".svc." (service DNS)
    % - Contains ".cluster.local" (default cluster domain)
    string:str(Hostname, ".svc.") > 0 orelse
    string:str(Hostname, ".cluster.local") > 0.

%% @private
%% Check if hostname is fully qualified Kubernetes service name
is_fully_qualified_k8s_name(Hostname) ->
    string:str(Hostname, ".svc.") > 0.

%% @private
%% Resolve Kubernetes service names with search domain expansion
resolve_k8s_hostname(Hostname) ->
    case is_fully_qualified_k8s_name(Hostname) of
        true ->
            % Already fully qualified, resolve directly
            resolve_standard_hostname(Hostname);
        false ->
            % Short name, try expansion with Kubernetes search domains
            resolve_k8s_short_name(Hostname)
    end.

%% @private
%% Try to resolve a short Kubernetes service name by expanding with search domains
-spec resolve_k8s_short_name(string()) -> {ok, [inet:ip_address()]} | {error, term()}.
resolve_k8s_short_name(ServiceName) ->
    K8sEnvironment = detect_k8s_environment(),
    SearchDomains = build_k8s_search_domains(ServiceName, K8sEnvironment),
    try_k8s_search_domains(SearchDomains, ServiceName).

%% @private
%% Try each search domain until one resolves
-spec try_k8s_search_domains([string()], string()) -> {ok, [inet:ip_address()]} | {error, term()}.
try_k8s_search_domains([], ServiceName) ->
    % All search domains failed, try the original name as fallback
    case resolve_standard_hostname(ServiceName) of
        {ok, _} = Success -> Success;
        {error, _} -> {error, {k8s_service_not_found, ServiceName}}
    end;
try_k8s_search_domains([Domain | Rest], ServiceName) ->
    FullName = ServiceName ++ "." ++ Domain,
    case resolve_standard_hostname(FullName) of
        {ok, _} = Success -> Success;
        {error, {hostname_not_found, _}} -> try_k8s_search_domains(Rest, ServiceName);
        {error, _} = OtherError -> OtherError
    end.

%% @private
%% Detect Kubernetes execution environment
detect_k8s_environment() ->
    case filelib:is_file("/var/run/secrets/kubernetes.io/serviceaccount/token") of
        true ->
            {in_cluster, read_current_namespace()};
        false ->
            case os:getenv("KUBERNETES_SERVICE_HOST") of
                false -> external;
                _ -> {external_with_k8s_env, os:getenv("KUBERNETES_NAMESPACE", "default")}
            end
    end.

%% @private
%% Read the current namespace from the service account
read_current_namespace() ->
    NamespaceFile = "/var/run/secrets/kubernetes.io/serviceaccount/namespace",
    case file:read_file(NamespaceFile) of
        {ok, NamespaceBin} ->
            string:trim(binary_to_list(NamespaceBin));
        {error, _} ->
            "default"  % Fallback to default namespace
    end.

%% @private
%% Build search domains for Kubernetes service resolution
build_k8s_search_domains(_ServiceName, K8sEnvironment) ->
    ClusterDomain = get_cluster_domain(),
    CurrentNamespace = get_current_namespace(K8sEnvironment),

    % Build search domains in order of preference:
    % 1. Current namespace
    % 2. Default namespace (if not current)
    % 3. kube-system namespace (for system services)
    SearchDomains = [
        CurrentNamespace ++ ".svc." ++ ClusterDomain
    ],

    % Add default namespace if different from current
    SearchDomains2 = case CurrentNamespace of
        "default" -> SearchDomains;
        _ -> SearchDomains ++ ["default.svc." ++ ClusterDomain]
    end,

    % Add kube-system for system services
    SearchDomains3 = case CurrentNamespace of
        "kube-system" -> SearchDomains2;
        _ -> SearchDomains2 ++ ["kube-system.svc." ++ ClusterDomain]
    end,

    SearchDomains3.

%% @private
%% Get cluster domain (usually cluster.local)
get_cluster_domain() ->
    case os:getenv("CLUSTER_DOMAIN") of
        false -> "cluster.local";
        Domain -> Domain
    end.

%% @private
%% Get current namespace based on environment
get_current_namespace({in_cluster, Namespace}) -> Namespace;
get_current_namespace({external_with_k8s_env, Namespace}) -> Namespace;
get_current_namespace(external) -> "default".

%% @private
%% Check if running in Kubernetes environment
-spec is_k8s_environment() -> boolean().
is_k8s_environment() ->
    detect_k8s_environment() /= external.
