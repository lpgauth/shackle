-module(shackle_backoff).
-include("shackle.hrl").

%% public
-export([
    timeout/2
]).

%% public
-spec timeout(pos_integer(), pos_integer()) -> pos_integer().

timeout(Time, Max) when Time >= Max ->
    Time;
timeout(Time, Max) ->
    Width = Time bsl 1,
    Time2 = Time + random:uniform(Width + 1) - 1,
    min(Time2, Max).
