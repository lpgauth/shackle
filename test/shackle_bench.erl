-module(shackle_bench).
-include("test.hrl").

-export([
    run/0
]).

-define(N, 10240).
-define(CLIENT, arithmetic_tcp_client).
-define(CONCURENCIES, [32, 64, 128, 512, 2048]).
-define(POOL_SIZES, [16, 32, 64, 128, 256]).

%% public
-spec run() ->
    ok.

run() ->
    error_logger:tty(false),
    {ok, _} = shackle_app:start(),
    io:format("Running benchmark...~n~n" ++
        "PoolSize  Concurency  Requests/s  Error %~n" ++
        [$= || _ <- lists:seq(1, 49)] ++ "~n", []),
    run_pool_size(?POOL_SIZES, ?CONCURENCIES, ?N).

%% private
lookup(Key, List) ->
    case lists:keyfind(Key, 1, List) of
        false -> undefined;
        {_, Value} -> Value
    end.

name(PoolSize, Concurency) ->
    list_to_atom("shackle_" ++ integer_to_list(PoolSize) ++
        "_" ++ integer_to_list(Concurency)).

run_pool_size([], _Concurencies, _N) ->
    ok;
run_pool_size([PoolSize | T], Concurencies, N) ->
    run_concurency(PoolSize, Concurencies, N),
    run_pool_size(T, Concurencies, N).

run_concurency(_PoolSize, [], _N) ->
    ok;
run_concurency(PoolSize, [Concurency | T], N) ->
    ok = ?CLIENT:start([
        {backlog_size, ?BACKLOG_SIZE},
        {pool_size, PoolSize}
    ]),
    timer:sleep(100),
    Fun = fun() ->
        case ?CLIENT:add(10, 10) of
            20 ->
                ok;
            {error, Reason} ->
                {error, Reason}
        end
    end,
    Name = name(PoolSize, Concurency),
    Results = timing_hdr:run(Fun, [
        {name, name},
        {concurrency, Concurency},
        {iterations, N},
        {output, "output/" ++ atom_to_list(Name)}
    ]),
    Qps = lookup(success, Results) / (lookup(total_time, Results) / 1000000),
    Errors = lookup(errors, Results) / lookup(iterations, Results) * 100,
    io:format("~7B ~11B ~11B ~8.1f~n",
        [PoolSize, Concurency, trunc(Qps), Errors]),
    ok = ?CLIENT:stop(),
    run_concurency(PoolSize, T, N).
