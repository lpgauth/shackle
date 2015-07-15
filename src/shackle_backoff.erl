-module(shackle_backoff).
-include("shackle.hrl").

%% public
-export([
    timeout/1
]).

%% public
-spec timeout(pos_integer()) -> pos_integer().

timeout(ConnectRetry) when ConnectRetry > 10 ->
    ?DEFAULT_CONNECT_RETRY * 10;
timeout(ConnectRetry) ->
    ?DEFAULT_CONNECT_RETRY * ConnectRetry.
