-include("shackle.hrl").
-include("shackle_defaults.hrl").

%% macros
-define(APP, shackle).
-define(CHILD(Mod), {Mod, {Mod, start_link, [Mod]}, permanent, 5000, worker, [Mod]}).
-define(GET_ENV(Key, Default), application:get_env(?APP, Key, Default)).
-define(LOOKUP(Key, List), ?LOOKUP(Key, List, undefined)).
-define(LOOKUP(Key, List, Default), shackle_utils:lookup(Key, List, Default)).
-define(MSG_CONNECT, connect).
-define(MSG_SWEEP, sweep).
-define(SERVER, shackle_server).
-define(SUPERVISOR, shackle_sup).

%% Put atomics slots that different schedulers write 128 bytes apart.
%% Slots that are next to each other share a cache line. A cache line
%% is 128 bytes on Apple silicon, and x86 prefetches lines in pairs.
-define(STRIDE, 16).

-include_lib("kernel/include/logger.hrl").
-define(EXCEPTION(Class, Reason, Stacktrace), Class:Reason:Stacktrace).
-define(GET_STACK(Stacktrace), Stacktrace).
-define(DEBUG(PoolName, Format, Data), ?LOG_DEBUG("[~p] " ++ Format, [PoolName | Data])).
-define(WARN(PoolName, Format, Data), ?LOG_WARNING("[~p] " ++ Format, [PoolName | Data])).

-record(pool_options, {
    backlog       :: atomics:atomics_ref(),
    backlog_size  :: shackle_backlog:backlog_size(),
    client        :: shackle:client(),
    max_retries   :: shackle_pool:max_retries(),
    name          :: shackle_pool:name(),
    pool_size     :: shackle_pool:pool_size(),
    pool_strategy :: shackle_pool:pool_strategy(),
    round_robin   :: undefined | atomics:atomics_ref(),
    server_names  :: tuple(),
    status        :: counters:counters_ref()
}).
