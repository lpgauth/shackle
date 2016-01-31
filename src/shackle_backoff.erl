-module(shackle_backoff).
-include("shackle_internal.hrl").

-compile({no_auto_import, [min/2]}).

%% public
-export([
    timeout/1
]).

%% public
-spec timeout(reconnect_state()) ->
    reconnect_state().

timeout(#reconnect_state {
        current = undefined,
        min = Min
    } = ReconnectState) ->

    timeout(ReconnectState#reconnect_state {
        current = Min
    });
timeout(#reconnect_state {
        current = Current,
        max = Max
    } = ReconnectState) when Max =/= infinity, Current >= Max ->

    ReconnectState;
timeout(#reconnect_state {
        current = Current,
        max = Max
    } = ReconnectState) ->

    Width = Current bsl 1,
    Current2 = Current + shackle_utils:random(Width + 1) - 1,

    ReconnectState#reconnect_state {
        current = min(Current2, Max)
    }.

%% private
min(A, infinity) ->
    A;
min(A, B) when B >= A ->
    A;
min(_, B) ->
    B.
