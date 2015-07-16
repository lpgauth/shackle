-module(shackle_backoff).
-include("shackle.hrl").

%% public
-export([
    timeout/1
]).

%% public
-spec timeout(pos_integer()) -> pos_integer().

timeout(N) when N >= ?DEFAULT_MAX_TIMEOUT ->
    ?DEFAULT_MAX_TIMEOUT;
timeout(N) ->
    Width = N bsl 1,
    N2 = N + random:uniform(Width + 1) - 1,
    min(N2, ?DEFAULT_MAX_TIMEOUT).
