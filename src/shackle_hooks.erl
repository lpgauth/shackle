-module(shackle_hooks).
-include("shackle_internal.hrl").

-dialyzer({nowarn_function, event/3}).
-ignore_xref([
    {shackle_hooks_foil, lookup, 1}
]).

-compile(inline).
-compile({inline_size, 512}).

-export([
    init/0,
    event/3
]).

%% callbacks
-optional_callbacks([event/3]).

-type event_name() :: [atom(), ...].
-type event_measurements() :: map().
-type event_metadata() :: map().

-callback event(event_name(), event_measurements(), event_metadata()) ->
    ok.

%% public
-spec init() ->
    ok.

init() ->
    Hooks = ?GET_ENV(hooks, []),
    Metrics = ?LOOKUP(metrics, Hooks, undefined),
    foil:new(?MODULE),
    foil:insert(?MODULE, metrics, Metrics),
    foil:load(?MODULE),
    ok.

-spec event(event_name(), event_measurements(), event_metadata()) ->
    ok.

event(EventName, Measurements, Metadata) ->
    case shackle_hooks_foil:lookup(metrics) of
        {ok, {M, F}} ->
            M:F(EventName, Measurements, Metadata);
        _ ->
            ok
    end.
