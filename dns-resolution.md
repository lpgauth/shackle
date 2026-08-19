# DNS Resolution in Shackle

## Overview

Shackle provides enhanced DNS resolution capabilities for reliable hostname and IP address handling. The DNS resolution system includes:

- Intelligent caching with 5-minute TTL
- Support for IPv4/IPv6 addresses
- Flexible input formats (strings, atoms, tuples)
- **Automatic Kubernetes service discovery** with zero configuration
- Clear error messages and helpful diagnostics

## Features

### 1. Multi-Format Hostname Support

The `shackle_servers:resolve_host/1` function accepts multiple input formats:

#### String Hostnames
```erlang
{ok, IPs} = shackle_servers:resolve_host("example.com").
{ok, IPs} = shackle_servers:resolve_host("localhost").
```

#### Atom Hostnames
```erlang
{ok, IPs} = shackle_servers:resolve_host(localhost).
{ok, IPs} = shackle_servers:resolve_host(my_server).
```

#### IPv4 Addresses
```erlang
% String format
{ok, [{127, 0, 0, 1}]} = shackle_servers:resolve_host("127.0.0.1").

% Tuple format
{ok, [{127, 0, 0, 1}]} = shackle_servers:resolve_host({127, 0, 0, 1}).
```

#### IPv6 Addresses
```erlang
% Tuple format only
{ok, [{0, 0, 0, 0, 0, 0, 0, 1}]} =
    shackle_servers:resolve_host({0, 0, 0, 0, 0, 0, 0, 1}).
```

### 2. DNS Caching

DNS resolution results are automatically cached to improve performance:

- **Cache TTL**: 5 minutes for successful resolutions
- **Per-Process Cache**: Uses Erlang process dictionary (no external storage)
- **Error Handling**: Failed resolutions are not cached (always retried)
- **Automatic Expiration**: Cached results expire after 5 minutes

#### Cache Behavior Examples

```erlang
% First call - performs DNS lookup
{ok, IPs1} = shackle_servers:resolve_host("example.com"),  % Hits network

% Second call within 5 minutes - uses cache
{ok, IPs2} = shackle_servers:resolve_host("example.com"),  % No network

% Clearing cache manually
erase({dns_cache, "example.com"}),

% Third call - fresh lookup
{ok, IPs3} = shackle_servers:resolve_host("example.com"),  % Hits network again
```

### 3. IPv4/IPv6 Resolution Strategy

The resolver prioritizes IPv4 but provides information about IPv6 availability:

```erlang
% IPv4 available - returns IPv4 addresses
{ok, [IPv4_Addr]} = shackle_servers:resolve_host("ipv4.example.com").

% Only IPv6 available - returns error indicating IPv6 not supported
{error, {ipv6_not_supported, IPv6_Addrs, Hostname}} =
    shackle_servers:resolve_host("ipv6_only.example.com").

% Hostname not found
{error, {hostname_not_found, "nonexistent.example.com"}} =
    shackle_servers:resolve_host("nonexistent.example.com").
```

### 4. Kubernetes Service Discovery

Shackle automatically detects and resolves Kubernetes service names with zero configuration.

#### Quick Start (Inside Kubernetes Cluster)

```erlang
% Short service names work automatically
shackle_pool:start(my_pool, my_client, [
    {pool_size, 4},
    {port, 9000},
    {servers, [
        "primary-service",              % Short name (auto-expanded)
        "backup-service.other-ns",      % Cross-namespace
        "cache.default.svc"             % Fully qualified
    ]}
]).
```

No configuration needed! Shackle automatically:
- Detects it's running in a Kubernetes cluster
- Reads the current namespace
- Expands short service names with search domains
- Resolves services across namespaces
- Caches results for 5 minutes

#### Automatic Detection

- Detects `.svc.` domain pattern (Kubernetes services)
- Detects `.cluster.local` domain (default K8s cluster domain)
- Detects if running in Kubernetes environment

#### Short Name Expansion

When running inside a Kubernetes cluster, short service names are automatically expanded with search domains:

```erlang
% Inside Kubernetes cluster
% These are all equivalent and will resolve to the same service:

{ok, IPs} = shackle_servers:resolve_host("my-service").
{ok, IPs} = shackle_servers:resolve_host("my-service.default").
{ok, IPs} = shackle_servers:resolve_host("my-service.default.svc.cluster.local").

% Attempts resolution in this order:
% 1. my-service.default.svc.cluster.local (current namespace)
% 2. my-service.default.svc.cluster.local (default namespace if different)
% 3. my-service.kube-system.svc.cluster.local (system services)
% 4. my-service (fallback to original name)
```

#### Kubernetes Environment Detection

Shackle detects Kubernetes environment via:

**In-Cluster Detection:**
- Checks for `/var/run/secrets/kubernetes.io/serviceaccount/token`
- Reads current namespace from `/var/run/secrets/kubernetes.io/serviceaccount/namespace`

**External K8s Detection:**
- Checks for `KUBERNETES_SERVICE_HOST` environment variable
- Reads namespace from `KUBERNETES_NAMESPACE` environment variable

**Not in Kubernetes:**
- Returns `external` immediately
- Standard DNS resolution used
- `is_k8s_environment()` returns false

#### Cross-Namespace Service Resolution

```erlang
% Resolve service in different namespace
{ok, IPs} = shackle_servers:resolve_host("my-service.other-namespace.svc").

% Fully qualified K8s service name
{ok, IPs} = shackle_servers:resolve_host("my-service.other-namespace.svc.cluster.local").
```

#### Environment Configuration

Kubernetes search domains can be customized via environment variables:

```bash
# Set custom cluster domain
export CLUSTER_DOMAIN="k8s.example.com"

# Set namespace (if auto-detection fails)
export KUBERNETES_NAMESPACE="production"
```

## Configuration Integration

The DNS resolution is used internally when shackle servers are configured. Multi-server configurations automatically resolve each server entry:

```erlang
shackle_pool:start(my_pool, my_client, [
    {pool_size, 4},
    {port, 9000},
    {servers, [
        "server1.example.com",      % Will be resolved to IPv4
        "server2.example.com",      % Results cached for 5 minutes
        "server3.example.com"
    ]}
]).
```

## API Reference

### shackle_servers:resolve_host/1

Resolves a hostname, IP address string, atom, or tuple to a list of IP addresses.

**Signature:**
```erlang
-spec resolve_host(Host :: string() | atom() |
                           {integer(), integer(), integer(), integer()} |
                           {integer(), integer(), integer(), integer(),
                            integer(), integer(), integer(), integer()})
    -> {ok, [inet:ip_address()]} | {error, term()}.
```

**Input:**
- String hostname: `"example.com"` or character code list
- Atom hostname: `localhost` or `my_server`
- IPv4 tuple: `{192, 168, 1, 1}`
- IPv6 tuple: `{0, 0, 0, 0, 0, 0, 0, 1}`
- IPv4 string: `"192.168.1.1"`
- Kubernetes services: `"my-service"` (auto-expanded in K8s)

**Output:**
- `{ok, [IP_Address, ...]}` - List of resolved IP addresses
- `{error, Reason}` - Resolution failed

**Errors:**
- `{hostname_not_found, Hostname}` - Hostname could not be resolved
- `{ipv6_not_supported, IPv6Addrs, Hostname}` - Only IPv6 available (not supported)
- `{k8s_service_not_found, ServiceName}` - K8s service not found in any search domain
- `{invalid_host_format, Host}` - Invalid input format

**Examples:**
```erlang
% Resolve hostname
{ok, IPs} = shackle_servers:resolve_host("example.com"),

% Parse IPv4 string
{ok, [{127, 0, 0, 1}]} = shackle_servers:resolve_host("127.0.0.1"),

% Kubernetes short name (automatically expands in K8s)
{ok, IPs} = shackle_servers:resolve_host("my-service"),

% Handle IPv6-only hostname
{error, {ipv6_not_supported, IPv6Addrs, "example.com"}} =
    shackle_servers:resolve_host("example.com").
```

### shackle_servers:resolve_hostname/1

Internal function that performs DNS resolution with automatic caching.

**Signature:**
```erlang
-spec resolve_hostname(Hostname :: string())
    -> {ok, [inet:ip_address()]} | {error, term()}.
```

**Note**: This function assumes input is a valid hostname string. Use `resolve_host/1` for external calls.

### shackle_servers:is_k8s_environment/0

Checks if the application is running in a Kubernetes environment.

**Signature:**
```erlang
-spec is_k8s_environment() -> boolean().
```

**Returns:**
- `true` if running in Kubernetes (in-cluster or with K8s env vars)
- `false` otherwise

**Example:**
```erlang
case shackle_servers:is_k8s_environment() of
    true -> io:format("Running in Kubernetes~n");
    false -> io:format("Not running in Kubernetes~n")
end.
```

### Kubernetes Service Helper Functions (Internal - Test/Debug)

The following functions are used internally for Kubernetes service discovery:

**`is_k8s_service_name(Hostname)`** - Detects if hostname looks like a K8s service
- Returns `true` if hostname contains `.svc.` or `.cluster.local`

**`is_fully_qualified_k8s_name(Hostname)`** - Detects if fully qualified
- Returns `true` if hostname contains `.svc.`

**`detect_k8s_environment()`** - Determines K8s environment type
- Returns `{in_cluster, Namespace}` if running inside cluster
- Returns `{external_with_k8s_env, Namespace}` if external K8s env vars set
- Returns `external` if not in Kubernetes

**`get_current_namespace(Environment)`** - Gets namespace from environment

**`get_cluster_domain()`** - Gets cluster domain (default: cluster.local)

**`build_k8s_search_domains(ServiceName, Environment)`** - Builds search domains

These functions are automatically used when a Kubernetes service name is detected.

## Usage Examples

### Basic DNS Resolution

```erlang
-module(my_app).

dns_example() ->
    % Resolve hostname
    {ok, IPs} = shackle_servers:resolve_host("example.com"),
    io:format("Resolved to: ~p~n", [IPs]).
```

### Multi-Server with DNS Resolution

```erlang
-module(my_pool_app).

start_pool() ->
    shackle_pool:start(my_pool, my_client, [
        {pool_size, 4},
        {port, 9000},
        {servers, [
            "primary.example.com",
            "secondary.example.com",
            "backup.example.com"
        ]}
    ]).

% Hostnames are automatically resolved
% Results cached for 5 minutes
% Automatic failover to next server on connection error
```

### Kubernetes Multi-Service Pool

```erlang
-module(k8s_pool).

start_pool() ->
    shackle_pool:start(my_pool, my_client, [
        {pool_size, 4},
        {port, 9000},
        {servers, [
            "api-service",              % In current namespace
            "cache-service",            % In current namespace
            "db-service.external.svc"   % In external namespace
        ]}
    ]).
```

### Direct Kubernetes Service Resolution

```erlang
% Resolve a single Kubernetes service
resolve_service() ->
    case shackle_servers:resolve_host("my-service") of
        {ok, IPs} ->
            io:format("Service resolved to: ~p~n", [IPs]);
        {error, Reason} ->
            io:format("Failed to resolve service: ~p~n", [Reason])
    end.
```

### Error Handling in Production

```erlang
-module(safe_resolver).

safe_resolve(Hostname) ->
    case shackle_servers:resolve_host(Hostname) of
        {ok, IPs} ->
            {ok, IPs};
        {error, {ipv6_not_supported, _IPv6Addrs, _Hostname}} ->
            % Hostname only has IPv6 - try alternate
            try_alternate_hostname();
        {error, {hostname_not_found, Hostname}} ->
            % DNS lookup failed - implement retry
            io:format("Could not resolve: ~s~n", [Hostname]),
            {error, dns_failure};
        {error, {k8s_service_not_found, ServiceName}} ->
            % K8s service not found - check if in K8s
            io:format("K8s service not found: ~s~n", [ServiceName]),
            {error, service_not_found};
        {error, Other} ->
            io:format("DNS error: ~p~n", [Other]),
            {error, dns_error}
    end.

try_alternate_hostname() ->
    % Implementation to try different hostname
    {error, no_ipv4_address}.
```

## Performance Considerations

### Cache Efficiency

DNS caching provides significant performance improvements:

- **Typical Case**: ~1ms with cache vs ~10-50ms for network lookup
- **Cache Hit Rate**: High for repeated connections to same servers
- **Memory Overhead**: ~200 bytes per cached entry
- **K8s Short Name Expansion**: +10-50ms per uncached lookup (tries multiple search domains)

### Optimization Tips

1. **Reuse Server Lists**: Keep `servers` list stable to maximize cache hits
2. **Batch Initialization**: Initialize pools before high-load periods
3. **Monitor Resolution**: Log DNS errors for troubleshooting
4. **Preload Cache**: Consider resolving service names during startup

## Compatibility

### Supported Erlang Versions
- OTP 19.0+ (uses `inet:parse_address/1` and `inet:getaddrs/2`)
- OTP 28+ recommended for best performance

### Network Support
- **IPv4**: Full support
- **IPv6**: Detection support (errors when IPv6-only)
- **Hostnames**: Standard DNS hostname resolution
- **Kubernetes**: 1.14+ with default cluster domain
- **Special Hosts**: `localhost` and other system-defined hostnames

### Kubernetes Versions
- Works with Kubernetes 1.14+
- Tested with default cluster domain (cluster.local)
- Supports custom cluster domains via `CLUSTER_DOMAIN` env var

## Error Handling

### Common Error Scenarios

**Hostname Not Found**
```erlang
{error, {hostname_not_found, "invalid.example.com"}} =
    shackle_servers:resolve_host("invalid.example.com").

% Handle by:
% 1. Verify hostname spelling
% 2. Check DNS configuration
% 3. Implement retry logic with backoff
```

**IPv6-Only Hostname**
```erlang
{error, {ipv6_not_supported, IPv6Addrs, Hostname}} =
    shackle_servers:resolve_host("ipv6_host.example.com").

% Handle by:
% 1. Use different hostname that has IPv4
% 2. Wait for Shackle IPv6 support
% 3. Use dual-stack hostname if available
```

**Invalid Input Format**
```erlang
{error, {invalid_host_format, invalid_input}} =
    shackle_servers:resolve_host(invalid_input).

% Handle by:
% 1. Validate input is string, atom, or IP tuple
% 2. Ensure numeric values in IP tuples are 0-255 (IPv4) or valid (IPv6)
```

**Kubernetes Service Not Found**
```erlang
{error, {k8s_service_not_found, "my-service"}} =
    shackle_servers:resolve_host("my-service").

% Handle by:
% 1. Check service exists: kubectl get svc -A | grep my-service
% 2. Verify namespace: kubectl config current-context
% 3. Try fully qualified name: my-service.default.svc.cluster.local
% 4. Check cluster domain: kubectl cluster-info | grep cluster.local
```

## Troubleshooting

### DNS Resolution Slow

**Problem**: DNS lookups taking too long

**Solutions**:
1. Verify DNS server is responsive: `dig example.com`
2. Check network connectivity
3. Look for DNS timeout issues
4. Consider using IP addresses instead of hostnames
5. Monitor cache hit rates

### Hostnames Not Resolving

**Problem**: Getting `{hostname_not_found, ...}` errors

**Solutions**:
1. Verify hostname is spelled correctly
2. Test with `nslookup example.com`
3. Check if running in isolated environment (container, VM)
4. Verify `/etc/resolv.conf` is configured correctly
5. Check network access to DNS servers

### IPv6-Only Hostnames

**Problem**: Error `{ipv6_not_supported, ...}`

**Solutions**:
1. Use hostname that has IPv4 addresses
2. Configure dual-stack DNS if available
3. Wait for Shackle IPv6 support in future versions
4. Use explicit IPv4 addresses instead

### Kubernetes Service Not Resolving

**Problem**: Getting `{k8s_service_not_found, "my-service"}`

**Solutions**:
1. Check service exists: `kubectl get svc -A | grep my-service`
2. Verify namespace: `kubectl config current-context`
3. Try fully qualified name: `my-service.default.svc.cluster.local`
4. Check cluster domain: `kubectl cluster-info | grep cluster.local`
5. Verify pod has service account mounted
6. Check environment variables: `env | grep KUBERNETES`

### Wrong Kubernetes Namespace Detected

**Problem**: Service resolving to wrong namespace

**Solutions**:
1. Explicitly set namespace: `export KUBERNETES_NAMESPACE="correct-ns"`
2. Use fully qualified name: `service.correct-ns.svc`
3. Check current pod's namespace: `cat /var/run/secrets/kubernetes.io/serviceaccount/namespace`

### Not Detecting Kubernetes Environment

**Problem**: `is_k8s_environment()` returns `false` inside K8s

**Solutions**:
1. Check pod has service account mounted
2. Verify `KUBERNETES_SERVICE_HOST` is set: `env | grep KUBERNETES`
3. Check token file exists: `ls /var/run/secrets/kubernetes.io/serviceaccount/`

## Future Enhancements

Potential improvements for future versions:

1. **Per-Service TTL**: Different cache TTL for K8s vs external services
2. **Headless Services**: Support for K8s headless service discovery
3. **DNS Search Domains**: Support for `/etc/resolv.conf` search domains
4. **Service Discovery**: Integration with Consul, Eureka, etc.
5. **IPv6 Support**: Native IPv6 connection support
6. **Custom Resolvers**: Plugin architecture for custom resolution strategies
7. **Metrics**: Track DNS resolution times and hit rates
8. **Health Checks**: Service health status verification

## Related Functions

- `shackle_servers:new/2` - Initialize multi-server configuration
- `shackle_servers:current/1` - Get current server address
- `inet:getaddrs/2` - Underlying Erlang DNS function
- `inet:parse_address/1` - Parse IP address strings

## References

- [Erlang `inet` Module Documentation](https://erlang.org/doc/man/inet.html)
- [Kubernetes DNS Documentation](https://kubernetes.io/docs/concepts/services-networking/dns-pod-service/)
- [Arterial DNS Resolution](https://github.com/lpgauth/arterial) (inspiration)
- [Multi-Server Support](MULTI_SERVER.md) - Uses DNS resolution
