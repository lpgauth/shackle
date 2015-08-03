%% records
-record(shackle_req, {
    cast           :: term(),
    client         :: client(),
    from           :: pid(),
    pool_name      :: pool_name(),
    ref            :: reference(),
    reply          :: term(),
    timestamp      :: erlang:timestamp(),
    timings   = [] :: [pos_integer()]
}).

%% types
-type backlog_size() :: pos_integer().
-type client() :: module().
-type client_option() :: {connect_options, [gen_tcp:connect_option()]} |
                         {ip, inet:ip_address() | inet:hostname()} |
                         {port, inet:port_number()} |
                         {reconnect, boolean()} |
                         {reconnect_time_max, time()} |
                         {reconnect_time_min, time()} |
                         {state, term()}.

-type client_options() :: [client_option()].
-type external_request_id() :: term().
-type pool_name() :: atom().
-type pool_option() :: {backlog_size, backlog_size()} |
                       {pool_size, pool_size()} |
                       {pool_strategy, pool_strategy()}.

-type pool_options() :: [pool_option()].
-type pool_size() :: pos_integer().
-type pool_strategy() :: random | round_robin.
-type server_name() :: atom().
-type shackle_req() :: #shackle_req {}.
-type shackle_req_id() :: {pool_name(), reference()}.
-type time() :: pos_integer().

-export_type([
    client_options/0,
    pool_options/0
]).
