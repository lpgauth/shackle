-module(shackle_telemetry).
-include("shackle_internal.hrl").

-compile(inline).
-compile({inline_size, 512}).

-export([
    backlog_full/1,
    disabled/1,
    found/1,
    handle_timeout/1,
    no_server/1,
    not_found/1,
    recv/2,
    replies/1,
    reply/2,
    send/2,
    timeout/1
]).

-spec backlog_full(client()) -> ok.
backlog_full(Client) ->
    Measurements = #{count => 1},
    Metadata = #{client => Client},
    telemetry:execute([shackle, backlog_full], Measurements, Metadata).

-spec disabled(client()) -> ok.
disabled(Client) ->
    Measurements = #{count => 1},
    Metadata = #{client => Client},
    telemetry:execute([shackle, disabled], Measurements, Metadata).

-spec found(client()) -> ok.
found(Client) ->
    Measurements = #{count => 1},
    Metadata = #{client => Client},
    telemetry:execute([shackle, found], Measurements, Metadata).

-spec handle_timeout(client()) -> ok.
handle_timeout(Client) ->
    Measurements = #{count => 1},
    Metadata = #{client => Client},
    telemetry:execute([shackle, handle_timeout], Measurements, Metadata).

-spec no_server(client()) -> ok.
no_server(Client) ->
    Measurements = #{count => 1},
    Metadata = #{client => Client},
    telemetry:execute([shackle, no_server], Measurements, Metadata).

-spec not_found(client()) -> ok.
not_found(Client) ->
    Measurements = #{count => 1},
    Metadata = #{client => Client},
    telemetry:execute([shackle, not_found], Measurements, Metadata).

-spec recv(client(), non_neg_integer()) -> ok.
recv(Client, NBytes) ->
    Measurements = #{count => 1, bytes => NBytes},
    Metadata = #{client => Client},
    telemetry:execute([shackle, recv], Measurements, Metadata).

-spec replies(client()) -> ok.
replies(Client) ->
    Measurements = #{count => 1},
    Metadata = #{client => Client},
    telemetry:execute([shackle, replies], Measurements, Metadata).

-spec reply(client(), non_neg_integer()) -> ok.
reply(Client, Microseconds) ->
    Measurements = #{duration => Microseconds},
    Metadata = #{client => Client},
    telemetry:execute([shackle, reply], Measurements, Metadata).

-spec send(client(), non_neg_integer()) -> ok.
send(Client, NBytes) ->
    Measurements = #{count => 1, bytes => NBytes},
    Metadata = #{client => Client},
    telemetry:execute([shackle, send], Measurements, Metadata).

-spec timeout(client()) -> ok.
timeout(Client) ->
    Measurements = #{count => 1},
    Metadata = #{client => Client},
    telemetry:execute([shackle, timeout], Measurements, Metadata).
