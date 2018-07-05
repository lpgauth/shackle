-include("shackle.hrl").
-include("shackle_defaults.hrl").

%% macros
-define(APP, shackle).
-define(LOOKUP(Key, List), ?LOOKUP(Key, List, undefined)).
-define(LOOKUP(Key, List, Default), shackle_utils:lookup(Key, List, Default)).
-define(MSG_CONNECT, connect).
-define(SERVER, shackle_server).
-define(SUPERVISOR, shackle_sup).
-define(SERVER_UTILS, shackle_server_utils).
-define(WARN(PoolName, Format, Data), shackle_utils:warning_msg(PoolName, Format, Data)).

%% ETS tables
-define(ETS_TABLE_BACKLOG, shackle_backlog).
-define(ETS_TABLE_POOL_INDEX, shackle_pool_index).
-define(ETS_TABLE_QUEUE, shackle_queue).
