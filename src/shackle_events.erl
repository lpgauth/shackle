-module(shackle_events).

-compile(inline).
-compile({inline_size, 512}).

-export([
    backlog_full/1,
    connected/3,
    connection_error/3,
    disabled/1,
    found/1,
    handle_timeout/1,
    no_server/1,
    not_found/1,
    queued_time/2,
    recv/2,
    replies/1,
    reply/4,
    send/2,
    timeout/2
]).

-define(EVENT(Block),
    case shackle_hooks:handler() of
        {M, F} ->
            {EventName, Measurements, Metadata} = Block,
            M:F(EventName, Measurements, Metadata);
        _ ->
            ok
    end
).

-spec backlog_full(shackle:client()) -> ok.
backlog_full(Client) ->
?EVENT(begin
    Measurements = #{count => 1},
    Metadata = #{client => Client},
    {[shackle, backlog_full], Measurements, Metadata}
end).

-spec connected(shackle:client(), shackle_pool:name(), integer()) -> ok.
connected(Client, PoolName, StartTime) ->
?EVENT(begin
    Measurements = #{count => 1, duration => duration_since(StartTime)},
    Metadata = #{client => Client, pool_name => PoolName},
    {[shackle, connected], Measurements, Metadata}
end).

-spec connection_error(shackle:client(), shackle_pool:name(), any()) -> ok.
connection_error(Client, PoolName, Reason) ->
?EVENT(begin
    Measurements = #{count => 1},
    Metadata = #{client => Client, pool_name => PoolName, reason => Reason},
    {[shackle, connection_error], Measurements, Metadata}
end).

-spec disabled(shackle:client()) -> ok.
disabled(Client) ->
?EVENT(begin
    Measurements = #{count => 1},
    Metadata = #{client => Client},
    {[shackle, disabled], Measurements, Metadata}
end).

-spec found(shackle:client()) -> ok.
found(Client) ->
?EVENT(begin
    Measurements = #{count => 1},
    Metadata = #{client => Client},
    {[shackle, found], Measurements, Metadata}
end).

-spec handle_timeout(shackle:client()) -> ok.
handle_timeout(Client) ->
?EVENT(begin
    Measurements = #{count => 1},
    Metadata = #{client => Client},
    {[shackle, handle_timeout], Measurements, Metadata}
end).

-spec no_server(shackle:client()) -> ok.
no_server(Client) ->
?EVENT(begin
    Measurements = #{count => 1},
    Metadata = #{client => Client},
    {[shackle, no_server], Measurements, Metadata}
end).

-spec not_found(shackle:client()) -> ok.
not_found(Client) ->
?EVENT(begin
    Measurements = #{count => 1},
    Metadata = #{client => Client},
    {[shackle, not_found], Measurements, Metadata}
end).

-spec queued_time(shackle:client(), non_neg_integer()) -> ok.
queued_time(Client, StartTime) ->
?EVENT(begin
    Measurements = #{duration => duration_since(StartTime)},
    Metadata = #{client => Client},
    {[shackle, queued_time], Measurements, Metadata}
end).

-spec recv(shackle:client(), iodata()) -> ok.
recv(Client, Data) ->
?EVENT(begin
    Measurements = #{count => 1, bytes => iolist_size(Data)},
    Metadata = #{client => Client},
    {[shackle, recv], Measurements, Metadata}
end).

-spec replies(shackle:client()) -> ok.
replies(Client) ->
?EVENT(begin
    Measurements = #{count => 1},
    Metadata = #{client => Client},
    {[shackle, replies], Measurements, Metadata}
end).

-spec reply(shackle:client(), term(), term(), integer()) -> ok.
reply(Client, Request, Response, Timestamp) ->
?EVENT(begin
    Measurements = #{duration => duration_since(Timestamp)},
    Metadata = #{client => Client, request => Request, response => Response},
    {[shackle, reply], Measurements, Metadata}
end).

-spec send(shackle:client(), iodata()) -> ok.
send(Client, Data) ->
?EVENT(begin
    Measurements = #{count => 1, bytes => iolist_size(Data)},
    Metadata = #{client => Client},
    {[shackle, send], Measurements, Metadata}
end).

-spec timeout(shackle:client(), term()) -> ok.
timeout(Client, Request) ->
?EVENT(begin
    Measurements = #{count => 1},
    Metadata = #{client => Client, request => Request},
    {[shackle, timeout], Measurements, Metadata}
end).

%% private

duration_since(Timestamp) ->
    erlang:monotonic_time() - Timestamp.
