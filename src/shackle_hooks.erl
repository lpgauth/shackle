-module(shackle_hooks).
-include("shackle_internal.hrl").

-dialyzer({nowarn_function, metrics/4}).
-ignore_xref([
    {shackle_hooks_foil, lookup, 1}
]).

-compile(inline).
-compile({inline_size, 512}).

-export([
    init/0,
    metrics/4
]).

%% callbacks
-optional_callbacks([metrics/4]).

-callback metrics(client(), metric_type(), metric_key(), metric_value()) ->
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

-spec metrics(client(), metric_type(), metric_key(), metric_value()) ->
    ok.

metrics(Client, Type, Key, Value) ->
    case shackle_hooks_foil:lookup(metrics) of
        {ok, {M, F}} ->
            M:F(Client, Type, Key, Value);
        _ ->
            ok
    end.
