-module(shackle_compiler).
-include("shackle_internal.hrl").

-export([
    pool_utils/1
]).

%% public
-spec pool_utils([{pool_name(), pool_options_rec()}]) ->
    ok.

pool_utils(Pools) ->
    Forms = pool_utils_forms(Pools),
    compile_and_load_forms(Forms),
    ok.

%% private
compile_and_load_forms(Forms) ->
    {ok, Module, Bin} = compile:forms(Forms, [debug_info]),
    code:purge(Module),
    Filename = atom_to_list(Module) ++ ".erl",
    {module, Module} = code:load_binary(Module, Filename, Bin),
    ok.

pool_utils_forms(Pools) ->
    Module = erl_syntax:attribute(erl_syntax:atom(module),
        [erl_syntax:atom(shackle_pool_utils)]),
    ExportList = [erl_syntax:arity_qualifier(erl_syntax:atom(options),
        erl_syntax:integer(1)),
        erl_syntax:arity_qualifier(erl_syntax:atom(server_name),
        erl_syntax:integer(2))],
    Export = erl_syntax:attribute(erl_syntax:atom(export),
        [erl_syntax:list(ExportList)]),
    Function1 = erl_syntax:function(erl_syntax:atom(options),
        options_clauses(Pools)),
    Function2 = erl_syntax:function(erl_syntax:atom(server_name),
        server_name_clauses(Pools)),
    Mod = [Module, Export, Function1, Function2],
    [erl_syntax:revert(X) || X <- Mod].

options_clause(PoolName, PoolOptions) ->
    Var1 = erl_syntax:atom(PoolName),
    Body = record(PoolOptions),
    erl_syntax:clause([Var1], [], [Body]).

options_clause_anon() ->
    Var = erl_syntax:variable("_"),
    Body = erl_syntax:atom(undefined),
    erl_syntax:clause([Var], [], [Body]).

options_clauses(Pools) ->
    options_clauses(Pools, []).

options_clauses([], Acc) ->
    Acc ++ [options_clause_anon()];
options_clauses([{PoolName, PoolOptions} | T], Acc) ->
    options_clauses(T, Acc ++ [options_clause(PoolName, PoolOptions)]).

record(Record) ->
    erl_syntax:tuple([to_syntax(X) || X <- tuple_to_list(Record)]).

server_atom(PoolName, Index) ->
    list_to_atom(atom_to_list(PoolName) ++ "_" ++ integer_to_list(Index)).

server_name_clause(PoolName, Index) ->
    Var1 = erl_syntax:atom(PoolName),
    Var2 = erl_syntax:integer(Index),
    Body = erl_syntax:atom(server_atom(PoolName, Index)),
    erl_syntax:clause([Var1, Var2], [], [Body]).

server_name_clause_anon() ->
    Var = erl_syntax:variable("_"),
    Body = erl_syntax:atom(undefined),
    erl_syntax:clause([Var, Var], [], [Body]).

server_name_clauses(Pools) ->
    server_name_clauses(Pools, []).

server_name_clauses([], Acc) ->
    Acc ++ [server_name_clause_anon()];
server_name_clauses([{PoolName, #pool_options {
        pool_size = PoolSize
    }} | T], Acc) ->

    ServerClauses = [server_name_clause(PoolName, Index) ||
        Index <- lists:seq(0, PoolSize)],
    server_name_clauses(T, Acc ++ ServerClauses).

to_syntax(X) when is_atom(X) ->
    erl_syntax:atom(X);
to_syntax(X) when is_integer(X) ->
    erl_syntax:integer(X).
