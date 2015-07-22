-module(shackle_utils).
-include("shackle.hrl").

%% public
-export([
    info_msg/3,
    lookup/3,
    timeout/2,
    warning_msg/3
]).

%% public
-spec info_msg(atom(), string(), [term()]) -> ok.

info_msg(Pool, Format, Data) ->
    ?IF_DEF_TEST(fun () ->
        error_logger:info_msg("[~p] " ++ Format, [Pool | Data])
    end).

-spec lookup(atom(), [{atom(), term()}], term()) -> term().

lookup(Key, List, Default) ->
    case lists:keyfind(Key, 1, List) of
        false -> Default;
        {_, Value} -> Value
    end.

-spec timeout(pos_integer(), erlang:timestamp()) -> integer().

timeout(Timeout, Timestamp) ->
    Diff = timer:now_diff(os:timestamp(), Timestamp) div 1000,
    Timeout - Diff.

-spec warning_msg(atom(), string(), [term()]) -> ok.

warning_msg(Pool, Format, Data) ->
    ?IF_DEF_TEST(fun () ->
        error_logger:warning_msg("[~p] " ++ Format, [Pool | Data])
    end).
