%% records
-record(cast, {
    request_id     :: request_id(),
    client         :: client(),
    pid            :: undefined | pid(),
    reply          :: undefined | term(),
    request        :: term(),
    timestamp      :: erlang:timestamp()
}).

%% types
-type backlog_size() :: pos_integer() | infinity.
-type cast() :: #cast {}.
-type client() :: module().
-type client_option() :: {ip, inet:ip_address() | inet:hostname()} |
                         {port, inet:port_number()} |
                         {protocol, protocol()} |
                         {reconnect, boolean()} |
                         {reconnect_time_max, time()} |
                         {reconnect_time_min, time()} |
                         {socket_options, [gen_tcp:connect_option() | gen_udp:option()]}.

-type client_options() :: [client_option()].
-type external_request_id() :: term().
-type pool_name() :: atom().
-type pool_option() :: {backlog_size, backlog_size()} |
                       {pool_size, pool_size()} |
                       {pool_strategy, pool_strategy()}.

-type pool_options() :: [pool_option()].
-type pool_size() :: pos_integer().
-type pool_strategy() :: random | round_robin.
-type protocol() :: shackle_tcp | shackle_udp.
-type response() :: {external_request_id(), term()}.
-type server_name() :: atom().
-type request_id() :: {server_name(), reference()}.
-type time() :: pos_integer().

-export_type([
    client_options/0,
    pool_options/0
]).
