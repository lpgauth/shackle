-module(shackle_utils).
-include("shackle_internal.hrl").

%% public
-export([
    info_msg/3,
    lookup/3,
    now_diff/1,
    timeout/2,
    timings/2,
    warning_msg/3
]).

%% public
-spec info_msg(pool_name(), string(), [term()]) -> ok.

info_msg(Pool, Format, Data) ->
    error_logger:info_msg("[~p] " ++ Format, [Pool | Data]).

-spec lookup(atom(), [{atom(), term()}], term()) -> term().

lookup(Key, List, Default) ->
    case lists:keyfind(Key, 1, List) of
        false -> Default;
        {_, Value} -> Value
    end.

-spec now_diff(erlang:timestamp()) -> non_neg_integer().

now_diff(Timestamp) ->
    timer:now_diff(os:timestamp(), Timestamp).

-spec timeout(time(), erlang:timestamp()) -> integer().

timeout(Timeout, Timestamp) ->
    Diff = timer:now_diff(os:timestamp(), Timestamp) div 1000,
    Timeout - Diff.

-spec timings(erlang:timestamp(), [non_neg_integer()]) -> [non_neg_integer()].

timings(Timestamp, []) ->
    [now_diff(Timestamp)];
timings(Timestamp, [H | _] = Timings) ->
    [(now_diff(Timestamp) - H) | Timings].

-spec warning_msg(pool_name(), string(), [term()]) -> ok.

warning_msg(Pool, Format, Data) ->
    error_logger:warning_msg("[~p] " ++ Format, [Pool | Data]).
