%% macros
-define(APP, shackle).
-define(DEFAULT_BACKLOG_SIZE, 1024).
-define(DEFAULT_MAX_TIMEOUT, 120000).
-define(DEFAULT_POOL_SIZE, 16).
-define(DEFAULT_POOL_STRATEGY, random).
-define(DEFAULT_RECONNECT, true).
-define(DEFAULT_SEND_TIMEOUT, 50).
-define(DEFAULT_TIMEOUT, 1000).
-define(ETS_TABLE_BACKLOG, shackle_backlog).
-define(ETS_TABLE_CACHE, shackle_cache).
-define(ETS_TABLE_POOL, shackle_pool).
-define(ETS_TABLE_QUEUE, shackle_queue).
-define(LOOKUP(Key, List), ?LOOKUP(Key, List, undefined)).
-define(LOOKUP(Key, List, Default), shackle_utils:lookup(Key, List, Default)).
-define(MSG_CONNECT, connect).
-define(MSG_TERMINATE, terminate).
-define(SUPERVISOR, shackle_sup).

-ifdef(TEST).
-define(IF_DEF_TEST, fun (_F) -> ok end).
-else.
-define(IF_DEF_TEST, fun (F) -> F() end).
-endif.

%% records
-record(pool_opts, {
    backlog_size,
    client,
    pool_size,
    pool_strategy
}).

%% types
-type init_opt() :: {ip, inet:ip_address() | inet:hostname()} |
                    {port, inet:port_number()} |
                    {reconnect, boolean()} |
                    {state, term()}.

-type init_opts() :: [init_opt()].

-type pool_strategy() :: random | round_robin.

-type pool_opt() :: {backlog_size, pos_integer()} |
                    {client, module()} |
                    {pool_size, pos_integer()} |
                    {pool_strategy, pool_strategy()}.

-type pool_opts() :: [pool_opt()].
