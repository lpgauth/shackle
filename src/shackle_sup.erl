-module(shackle_sup).
-include("shackle_internal.hrl").

%% internal
-export([
    start_link/0
]).

-behaviour(supervisor).
-export([
    init/1
]).

%% internal
-spec start_link() ->
    {ok, pid()}.

start_link() ->
    supervisor:start_link({local, ?SUPERVISOR}, ?SUPERVISOR, []).

%% supervisor callbacks
-spec init([]) ->
    {ok, {{one_for_one, 5, 10}, []}}.

init([]) ->
    shackle_backlog:init(),
    shackle_pool:init(),
    shackle_queue:init(),

    {ok, {{one_for_one, 5, 10}, []}}.
