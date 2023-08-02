-module(shackle_telemetry).

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

-spec backlog_full(shackle:client()) -> ok.
backlog_full(Client) ->
    Measurements = #{count => 1},
    Metadata = #{client => Client},
    telemetry:execute([shackle, backlog_full], Measurements, Metadata).

-spec connected(shackle:client(), shackle_pool:name(), non_neg_integer()) -> ok.
connected(Client, PoolName, Microseconds) ->
    Measurements = #{count => 1, duration => Microseconds},
    Metadata = #{client => Client, pool_name => PoolName},
    telemetry:execute([shackle, connected], Measurements, Metadata).

-spec connection_error(shackle:client(), shackle_pool:name(), any()) -> ok.
connection_error(Client, PoolName, Reason) ->
    Measurements = #{count => 1},
    Metadata = #{client => Client, pool_name => PoolName, reason => Reason},
    telemetry:execute([shackle, connection_error], Measurements, Metadata).

-spec disabled(shackle:client()) -> ok.
disabled(Client) ->
    Measurements = #{count => 1},
    Metadata = #{client => Client},
    telemetry:execute([shackle, disabled], Measurements, Metadata).

-spec found(shackle:client()) -> ok.
found(Client) ->
    Measurements = #{count => 1},
    Metadata = #{client => Client},
    telemetry:execute([shackle, found], Measurements, Metadata).

-spec handle_timeout(shackle:client()) -> ok.
handle_timeout(Client) ->
    Measurements = #{count => 1},
    Metadata = #{client => Client},
    telemetry:execute([shackle, handle_timeout], Measurements, Metadata).

-spec no_server(shackle:client()) -> ok.
no_server(Client) ->
    Measurements = #{count => 1},
    Metadata = #{client => Client},
    telemetry:execute([shackle, no_server], Measurements, Metadata).

-spec not_found(shackle:client()) -> ok.
not_found(Client) ->
    Measurements = #{count => 1},
    Metadata = #{client => Client},
    telemetry:execute([shackle, not_found], Measurements, Metadata).

-spec queued_time(shackle:client(), non_neg_integer()) -> ok.
queued_time(Client, Microseconds) ->
    Measurements = #{duration => Microseconds},
    Metadata = #{client => Client},
    telemetry:execute([shackle, queued_time], Measurements, Metadata).

-spec recv(shackle:client(), non_neg_integer()) -> ok.
recv(Client, NBytes) ->
    Measurements = #{count => 1, bytes => NBytes},
    Metadata = #{client => Client},
    telemetry:execute([shackle, recv], Measurements, Metadata).

-spec replies(shackle:client()) -> ok.
replies(Client) ->
    Measurements = #{count => 1},
    Metadata = #{client => Client},
    telemetry:execute([shackle, replies], Measurements, Metadata).

-spec reply(shackle:client(), term(), term(), non_neg_integer()) -> ok.
reply(Client, Request, Response, Microseconds) ->
    Measurements = #{duration => Microseconds},
    Metadata = #{client => Client, request => Request, response => Response},
    telemetry:execute([shackle, reply], Measurements, Metadata).

-spec send(shackle:client(), non_neg_integer()) -> ok.
send(Client, NBytes) ->
    Measurements = #{count => 1, bytes => NBytes},
    Metadata = #{client => Client},
    telemetry:execute([shackle, send], Measurements, Metadata).

-spec timeout(shackle:client(), term()) -> ok.
timeout(Client, Request) ->
    Measurements = #{count => 1},
    Metadata = #{client => Client, request => Request},
    telemetry:execute([shackle, timeout], Measurements, Metadata).
