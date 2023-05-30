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
-define(ETS_TABLE_STATUS, shackle_status).

%% compatibility
-ifdef(OTP_RELEASE). %% OTP-21+
-define(EXCEPTION(Class, Reason, Stacktrace), Class:Reason:Stacktrace).
-define(GET_STACK(Stacktrace), Stacktrace).

-include_lib("kernel/include/logger.hrl").
-define(DEBUG(PoolName, Format, Data), ?LOG_DEBUG("[~p] " ++ Format, [PoolName | Data])).
-define(WARN(PoolName, Format, Data), ?LOG_WARNING("[~p] " ++ Format, [PoolName | Data])).
-else.
-define(EXCEPTION(Class, Reason, _), Class:Reason).
-define(GET_STACK(_), erlang:get_stacktrace()).

% HACK: error_logger doesn't support DEBUG level so we use INFO instead
-define(DEBUG(PoolName, Format, Data), shackle_utils:info_msg(PoolName, Format, Data)).
-define(WARN(PoolName, Format, Data), shackle_utils:warning_msg(PoolName, Format, Data)).
-endif.
