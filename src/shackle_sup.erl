-module(shackle_sup).
-include("shackle.hrl").

-export([
    start_link/0
]).

-behaviour(supervisor).
-export([
    init/1
]).

%% public
-spec start_link() -> {ok, pid()}.

start_link() ->
    supervisor:start_link({local, ?SUPERVISOR}, ?SUPERVISOR, []).

%% supervisor callbacks
init([]) ->
    shackle_backlog:init(),
    shackle_cache:init(),
    shackle_queue:init(),

    {ok, {{one_for_one, 5, 10}, []}}.
