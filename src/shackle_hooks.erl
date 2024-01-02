-module(shackle_hooks).
-include("shackle_internal.hrl").

-dialyzer({nowarn_function, handler/0}).
-ignore_xref([
    {shackle_hooks_foil, lookup, 1}
]).

-compile(inline).
-compile({inline_size, 512}).

-export([
    init/0,
    handler/0
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
    Events = ?LOOKUP(events, Hooks, undefined),
    foil:new(?MODULE),
    foil:insert(?MODULE, events, Events),
    foil:load(?MODULE),
    ok.

-spec handler() -> {atom(), atom()} | undefined.
handler() ->
    case shackle_hooks_foil:lookup(events) of
        {ok, {M, F}} -> {M, F};
        _ -> undefined
    end.
