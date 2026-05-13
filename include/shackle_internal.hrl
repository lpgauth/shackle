-include("shackle.hrl").
-include("shackle_defaults.hrl").

%% macros
-define(APP, shackle).
-define(CHILD(Mod), {Mod, {Mod, start_link, [Mod]}, permanent, 5000, worker, [Mod]}).
-define(GET_ENV(Key, Default), application:get_env(?APP, Key, Default)).
-define(LOOKUP(Key, List), ?LOOKUP(Key, List, undefined)).
-define(LOOKUP(Key, List, Default), shackle_utils:lookup(Key, List, Default)).
-define(MSG_CONNECT, connect).
-define(SERVER, shackle_server).
-define(SUPERVISOR, shackle_sup).

%% ETS tables
-define(ETS_TABLE_POOL_INDEX, shackle_pool_index).

-include_lib("kernel/include/logger.hrl").
-define(EXCEPTION(Class, Reason, Stacktrace), Class:Reason:Stacktrace).
-define(GET_STACK(Stacktrace), Stacktrace).
-define(DEBUG(PoolName, Format, Data), ?LOG_DEBUG("[~p] " ++ Format, [PoolName | Data])).
-define(WARN(PoolName, Format, Data), ?LOG_WARNING("[~p] " ++ Format, [PoolName | Data])).
