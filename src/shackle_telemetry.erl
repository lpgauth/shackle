-module(shackle_telemetry).

-export([
    backlog_full/1,
    disabled/1,
    handle_timeout/1,
    no_server/1,
    not_found/1,
    recv/2,
    reply/2,
    send/2,
    timeout/1
]).

%% Events from the caller process read the application switch.
%% shackle_server reads the switch one time at init and calls the
%% other events only when the switch is on.
-spec backlog_full(shackle:client()) -> ok.
backlog_full(Client) ->
    execute_enabled([shackle, backlog_full], #{count => 1}, Client).

-spec disabled(shackle:client()) -> ok.
disabled(Client) ->
    execute_enabled([shackle, disabled], #{count => 1}, Client).

-spec handle_timeout(shackle:client()) -> ok.
handle_timeout(Client) ->
    execute([shackle, handle_timeout], #{count => 1}, Client).

-spec no_server(shackle:client()) -> ok.
no_server(Client) ->
    execute_enabled([shackle, no_server], #{count => 1}, Client).

-spec not_found(shackle:client()) -> ok.
not_found(Client) ->
    execute([shackle, not_found], #{count => 1}, Client).

-spec recv(shackle:client(), non_neg_integer()) -> ok.
recv(Client, NBytes) ->
    execute([shackle, recv], #{count => 1, bytes => NBytes}, Client).

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
    telemetry:execute(Event, Measurements, #{client => Client}).

execute_enabled(Event, Measurements, Client) ->
    case persistent_term:get({shackle, telemetry}, true) of
        true ->
            execute(Event, Measurements, Client);
        false ->
            ok
    end.
