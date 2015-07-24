%% macros
-define(APP, shackle).
-define(LOOKUP(Key, List), ?LOOKUP(Key, List, undefined)).
-define(LOOKUP(Key, List, Default), shackle_utils:lookup(Key, List, Default)).
-define(MSG_CONNECT, connect).
-define(SERVER, shackle_server).
-define(SUPERVISOR, shackle_sup).

-ifdef(TEST).
-define(IF_DEF_TEST, fun (_) -> ok end).
-else.
-define(IF_DEF_TEST, fun (F) -> F() end).
-endif.

%% defaults
-define(DEFAULT_BACKLOG_SIZE, 1024).
-define(DEFAULT_IP, "127.0.0.1").
-define(DEFAULT_MAX_TIMEOUT, 120000).
-define(DEFAULT_POOL_SIZE, 16).
-define(DEFAULT_POOL_STRATEGY, random).
-define(DEFAULT_RECONNECT, true).
-define(DEFAULT_RECONNECT_MAX, timer:minutes(2)).
-define(DEFAULT_RECONNECT_MIN, timer:seconds(1)).
-define(DEFAULT_CONNECT_OPTS, [
    {send_timeout, 50},
    {send_timeout_close, true}
]).
-define(DEFAULT_TIMEOUT, 1000).

%% ETS tables
-define(ETS_TABLE_BACKLOG, shackle_backlog).
-define(ETS_TABLE_POOL, shackle_pool).
-define(ETS_TABLE_QUEUE, shackle_queue).

%% records
-record(pool_options, {
    backlog_size,
    client,
    pool_size,
    pool_strategy
}).

%% types
-type client_option() :: {connect_options, [gen_tcp:connect_option()]} |
                         {ip, inet:ip_address() | inet:hostname()} |
                         {port, inet:port_number()} |
                         {reconnect, boolean()} |
                         {reconnect_max, pos_integer()} |
                         {reconnect_min, pos_integer()} |
                         {state, term()}.

-type client_options() :: [client_option()].

-type pool_strategy() :: random | round_robin.

-type pool_option() :: {backlog_size, pos_integer()} |
                       {pool_size, pos_integer()} |
                       {pool_strategy, pool_strategy()}.

-type pool_options() :: [pool_option()].

-export_type([
    client_options/0,
    pool_options/0
]).
