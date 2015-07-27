%% macros
-define(APP, shackle).
-define(LOOKUP(Key, List), ?LOOKUP(Key, List, undefined)).
-define(LOOKUP(Key, List, Default), shackle_utils:lookup(Key, List, Default)).
-define(MSG_CONNECT, connect).
-define(SERVER, shackle_server).
-define(SUPERVISOR, shackle_sup).

%% defaults
-define(DEFAULT_BACKLOG_SIZE, 1024).
-define(DEFAULT_CONNECT_OPTS, [
    {send_timeout, 50},
    {send_timeout_close, true}
]).
-define(DEFAULT_IP, "127.0.0.1").
-define(DEFAULT_MAX_TIMEOUT, 120000).
-define(DEFAULT_POOL_SIZE, 16).
-define(DEFAULT_POOL_STRATEGY, random).
-define(DEFAULT_RECONNECT, true).
-define(DEFAULT_RECONNECT_MAX, timer:minutes(2)).
-define(DEFAULT_RECONNECT_MIN, timer:seconds(1)).
-define(DEFAULT_TIMEOUT, 1000).

%% ETS tables
-define(ETS_TABLE_BACKLOG, shackle_backlog).
-define(ETS_TABLE_POOL, shackle_pool).
-define(ETS_TABLE_QUEUE, shackle_queue).

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
-type pool_name() :: atom().
-type pool_option() :: {backlog_size, backlog_size()} |
                       {pool_size, pool_size()} |
                       {pool_strategy, pool_strategy()}.

-type pool_options() :: [pool_option()].
-type pool_size() :: pos_integer().
-type pool_strategy() :: random | round_robin.
-type request() :: term().
-type request_id() :: term().
-type server_name() :: atom().
-type time() :: pos_integer().

-export_type([
    client_options/0,
    pool_options/0
]).

%% records
-record(pool_options, {
    backlog_size  :: backlog_size(),
    client        :: client(),
    pool_size     :: pool_size(),
    pool_strategy :: pool_strategy()
}).
