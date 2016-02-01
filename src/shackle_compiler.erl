-module(shackle_compiler).
-include("shackle_internal.hrl").

-export([
    pool_utils/1
]).

%% public
-spec pool_utils([{pool_name(), pool_size()}]) ->
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
    ExportList = [erl_syntax:arity_qualifier(erl_syntax:atom(server_name),
        erl_syntax:integer(2))],
    Export = erl_syntax:attribute(erl_syntax:atom(export),
        [erl_syntax:list(ExportList)]),
    Function = erl_syntax:function(erl_syntax:atom(server_name),
        server_name_clauses(Pools)),
    [erl_syntax:revert(X) || X <- [Module, Export, Function]].

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
server_name_clauses([{PoolName, PoolSize} | T], Acc) ->
    ServerClauses = [server_name_clause(PoolName, Index) ||
        Index <- lists:seq(0, PoolSize)],
    server_name_clauses(T, Acc ++ ServerClauses).
