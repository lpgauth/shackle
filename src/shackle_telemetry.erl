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
    shackle_hooks:event([shackle, backlog_full], Measurements, Metadata).

-spec connected(shackle:client(), shackle_pool:name(), non_neg_integer()) -> ok.
connected(Client, PoolName, Duration) ->
    Measurements = #{count => 1, duration => Duration},
    Metadata = #{client => Client, pool_name => PoolName},
    shackle_hooks:event([shackle, connected], Measurements, Metadata).

-spec connection_error(shackle:client(), shackle_pool:name(), any()) -> ok.
connection_error(Client, PoolName, Reason) ->
    Measurements = #{count => 1},
    Metadata = #{client => Client, pool_name => PoolName, reason => Reason},
    shackle_hooks:event([shackle, connection_error], Measurements, Metadata).

-spec disabled(shackle:client()) -> ok.
disabled(Client) ->
    Measurements = #{count => 1},
    Metadata = #{client => Client},
    shackle_hooks:event([shackle, disabled], Measurements, Metadata).

-spec found(shackle:client()) -> ok.
found(Client) ->
    Measurements = #{count => 1},
    Metadata = #{client => Client},
    shackle_hooks:event([shackle, found], Measurements, Metadata).

-spec handle_timeout(shackle:client()) -> ok.
handle_timeout(Client) ->
    Measurements = #{count => 1},
    Metadata = #{client => Client},
    shackle_hooks:event([shackle, handle_timeout], Measurements, Metadata).

-spec no_server(shackle:client()) -> ok.
no_server(Client) ->
    Measurements = #{count => 1},
    Metadata = #{client => Client},
    shackle_hooks:event([shackle, no_server], Measurements, Metadata).

-spec not_found(shackle:client()) -> ok.
not_found(Client) ->
    Measurements = #{count => 1},
    Metadata = #{client => Client},
    shackle_hooks:event([shackle, not_found], Measurements, Metadata).

-spec queued_time(shackle:client(), non_neg_integer()) -> ok.
queued_time(Client, Duration) ->
    Measurements = #{duration => Duration},
    Metadata = #{client => Client},
    shackle_hooks:event([shackle, queued_time], Measurements, Metadata).

-spec recv(shackle:client(), non_neg_integer()) -> ok.
recv(Client, NBytes) ->
    Measurements = #{count => 1, bytes => NBytes},
    Metadata = #{client => Client},
    shackle_hooks:event([shackle, recv], Measurements, Metadata).

-spec replies(shackle:client()) -> ok.
replies(Client) ->
    Measurements = #{count => 1},
    Metadata = #{client => Client},
    shackle_hooks:event([shackle, replies], Measurements, Metadata).

-spec reply(shackle:client(), term(), term(), non_neg_integer()) -> ok.
reply(Client, Request, Response, Duration) ->
    Measurements = #{duration => Duration},
    Metadata = #{client => Client, request => Request, response => Response},
    shackle_hooks:event([shackle, reply], Measurements, Metadata).

-spec send(shackle:client(), non_neg_integer()) -> ok.
send(Client, NBytes) ->
    Measurements = #{count => 1, bytes => NBytes},
    Metadata = #{client => Client},
    shackle_hooks:event([shackle, send], Measurements, Metadata).

-spec timeout(shackle:client(), term()) -> ok.
timeout(Client, Request) ->
    Measurements = #{count => 1},
    Metadata = #{client => Client, request => Request},
    shackle_hooks:event([shackle, timeout], Measurements, Metadata).
