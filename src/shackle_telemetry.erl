-module(shackle_telemetry).

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

-spec backlog_full(shackle:client()) -> ok.
backlog_full(Client) ->
    execute([shackle, backlog_full], #{count => 1}, Client).

-spec disabled(shackle:client()) -> ok.
disabled(Client) ->
    execute([shackle, disabled], #{count => 1}, Client).

-spec found(shackle:client()) -> ok.
found(Client) ->
    execute([shackle, found], #{count => 1}, Client).

-spec handle_timeout(shackle:client()) -> ok.
handle_timeout(Client) ->
    execute([shackle, handle_timeout], #{count => 1}, Client).

-spec no_server(shackle:client()) -> ok.
no_server(Client) ->
    execute([shackle, no_server], #{count => 1}, Client).

-spec not_found(shackle:client()) -> ok.
not_found(Client) ->
    execute([shackle, not_found], #{count => 1}, Client).

-spec recv(shackle:client(), non_neg_integer()) -> ok.
recv(Client, NBytes) ->
    execute([shackle, recv], #{count => 1, bytes => NBytes}, Client).

-spec replies(shackle:client()) -> ok.
replies(Client) ->
    execute([shackle, replies], #{count => 1}, Client).

-spec reply(shackle:client(), non_neg_integer()) -> ok.
reply(Client, Microseconds) ->
    execute([shackle, reply], #{duration => Microseconds}, Client).

-spec send(shackle:client(), non_neg_integer()) -> ok.
send(Client, NBytes) ->
    execute([shackle, send], #{count => 1, bytes => NBytes}, Client).

-spec timeout(shackle:client()) -> ok.
timeout(Client) ->
    execute([shackle, timeout], #{count => 1}, Client).

%% private
execute(Event, Measurements, Client) ->
    case persistent_term:get({shackle, telemetry}, true) of
        true ->
            telemetry:execute(Event, Measurements, #{client => Client});
        false ->
            ok
    end.
