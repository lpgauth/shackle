-include("shackle.hrl").

%% macros
-define(APP, shackle).
-define(LOOKUP(Key, List), ?LOOKUP(Key, List, undefined)).
-define(LOOKUP(Key, List, Default), shackle_utils:lookup(Key, List, Default)).
-define(MSG_CONNECT, connect).
-define(SERVER, shackle_server).
-define(SUPERVISOR, shackle_sup).

%% defaults
-define(DEFAULT_BACKLOG_SIZE, 1024).
-define(DEFAULT_IP, "127.0.0.1").
-define(DEFAULT_POOL_SIZE, 16).
-define(DEFAULT_POOL_STRATEGY, round_robin).
-define(DEFAULT_PROTOCOL, shackle_tcp).
-define(DEFAULT_RECONNECT, true).
-define(DEFAULT_RECONNECT_MAX, timer:minutes(2)).
-define(DEFAULT_RECONNECT_MIN, timer:seconds(1)).
-define(DEFAULT_SOCKET_OPTS, []).
-define(DEFAULT_TIMEOUT, 1000).

%% ETS tables
-define(ETS_TABLE_BACKLOG, shackle_backlog).
-define(ETS_TABLE_POOL, shackle_pool).
-define(ETS_TABLE_POOL_INDEX, shackle_pool_index).
-define(ETS_TABLE_QUEUE, shackle_queue).
-define(ETS_TABLE_QUEUE_REVERSE, shackle_queue_reverse).
