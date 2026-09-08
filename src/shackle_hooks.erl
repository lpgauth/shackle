-module(shackle_hooks).
-include("shackle_internal.hrl").

-ignore_xref([{shackle_hooks_foil, lookup, 1}]).
-dialyzer([{nowarn_function, [metrics/4]}]).

-export([
    init/0,
    metrics/4
]).

%% callbacks
-optional_callbacks([metrics/4]).

-callback metrics(shackle:client(), shackle:metric_type(), shackle:metric_key(), shackle:metric_value()) ->
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

-spec metrics(shackle:client(), shackle:metric_type(), shackle:metric_key(), shackle:metric_value()) ->
    ok.

metrics(Client, Type, Key, Value) ->
    case shackle_hooks_foil:lookup(metrics) of
        {ok, {M, F}} ->
            M:F(Client, Type, Key, Value);
        _ ->
            ok
    end.
