-module(shackle_backoff).
-include("shackle_internal.hrl").

%% public
-export([
    timeout/2
]).

%% public
-spec timeout(integer(), integer()) ->
    integer().

timeout(Time, MaxTime) when Time >= MaxTime ->
    Time;
timeout(Time, MaxTime) ->
    Width = Time bsl 1,
    Time2 = Time + random:uniform(Width + 1) - 1,
    min(Time2, MaxTime).
