-module(shackle_ets_manager).
-include("shackle_internal.hrl").

-export([
    start_link/1
]).

-behaviour(metal).
-export([
    handle_msg/2
]).

-spec start_link(atom()) ->
    {ok, pid()}.

start_link(Name) ->
    metal:start_link(?MODULE, Name, undefined).

%% metal callbacks
-spec handle_msg({'ETS-TRANSFER', atom(), ets:tid(), undefined}, undefined) ->
    {ok, undefined}.

handle_msg({'ETS-TRANSFER', _Table, _Tid, undefined}, State) ->
    {ok, State}.
