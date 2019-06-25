%% records
-record(cast, {
    client         :: client(),
    pid            :: undefined | pid(),
    request_id     :: request_id(),
    timeout        :: timeout(),
    timestamp      :: erlang:timestamp()
}).

-record(pool_options, {
    backlog_size  :: backlog_size(),
    client        :: client(),
    pool_size     :: pool_size(),
    pool_strategy :: pool_strategy(),
    %% the percentage of failed worker which will trigger pool_failure_callback_fn
    %% 0 mean any worker failure will trigger that callback.
    pool_failure_threshold_percentage = 0.0 :: float(),
    %% the percentage of failed worker which will trigger pool_recover_callback_fn
    %% 0 mean any worker failure will NOT trigger that callback.
    pool_recover_threshold_percentage = 0.0 :: float(),
    pool_failure_callback_fn :: fun(),
    pool_recover_callback_fn :: fun()
}).

-record(reconnect_state, {
    current :: undefined | time(),
    max     :: time() | infinity,
    min     :: time()
}).

%% types
-type backlog_size() :: pos_integer() | infinity.
-type cast() :: #cast {}.
-type client() :: module().
-type client_option() :: {init_options, init_options()} |
                         {ip, inet:ip_address() | inet:hostname()} |
                         {port, inet:port_number()} |
                         {protocol, protocol()} |
                         {reconnect, boolean()} |
                         {reconnect_time_max, time() | infinity} |
                         {reconnect_time_min, time()} |
                         {socket_options, [gen_tcp:connect_option() | gen_udp:option()]}.

-type client_options() :: [client_option()].
-type client_state() :: term().
-type external_request_id() :: term().
-type init_options() :: term().
-type pool_name() :: atom().
-type pool_option() :: {backlog_size, backlog_size()} |
                       {pool_size, pool_size()} |
                       {pool_strategy, pool_strategy()}.

-type pool_options() :: [pool_option()].
-type pool_options_rec() :: #pool_options {}.
-type pool_size() :: pos_integer().
-type pool_strategy() :: random | round_robin.
-type protocol() :: shackle_ssl| shackle_tcp | shackle_udp.
-type reconnect_state() :: #reconnect_state {}.
-type request_id() :: {server_name(), reference()}.
-type response() :: {external_request_id(), term()}.
-type server_name() :: atom().
-type socket() :: inet:socket() | ssl:sslsocket().
-type socket_type() :: inet | ssl.
-type time() :: pos_integer().

-export_type([
    client_options/0,
    init_options/0,
    pool_options/0,
    request_id/0
]).
