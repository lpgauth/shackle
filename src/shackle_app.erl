-module(shackle_app).
-include("shackle.hrl").

-export([
    start/0
]).

-behaviour(application).
-export([
    start/2,
    stop/1
]).

%% public
-spec start() -> ok.

start() ->
    {ok, _} = application:ensure_all_started(?APP),
    ok.

%% application callbacks
start(_StartType, _StartArgs) ->
    shackle_sup:start_link().

stop(_State) ->
    ok.
