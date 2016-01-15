-module(shackle_generator).
-include("shackle_internal.hrl").

-export([
    pool_utils/1
]).

-define(ABSTRACT(Servers, Line), [
    {attribute, 1, module, shackle_pool_utils},
    {attribute, 2, export, [{server_name, 2}]},
    {function, 3, server_name, 2, Servers},
    {eof, Line + 1}
]).

%% public
-spec pool_utils([{pool_name(), pool_size()}]) -> ok.

pool_utils(Pools) ->
    Abstract = abstract(Pools),
    {ok, Module, Bin} = compile:forms(Abstract, [verbose, report_errors]),
    {module, Module} = code:load_binary(Module, "shackle_pool_utils.erl", Bin),
    code:soft_purge(Module),
    ok.

%% private
abstract(Pools) ->
    {Servers, Line} = server_clauses(Pools, [], 4),
    ?ABSTRACT(Servers, Line).

abstract_clause(Ps, Guard, Body, Line) ->
    {clause, Line, Ps, Guard, Body}.

server_atom(PoolName, Index) ->
    list_to_atom(atom_to_list(PoolName) ++ "_" ++ integer_to_list(Index)).

server_clause(PoolName, Index, Line) ->
    Ps = server_clause_ps(PoolName, Index, Line),
    Body = server_clause_body(PoolName, Index, Line),
    abstract_clause(Ps, [], Body, Line).

server_clauses([], Acc, Line) ->
    {lists:flatten(lists:reverse(Acc)), Line};
server_clauses([{PoolName, PoolSize} | T], Acc, Line) ->
    ServerClauses = [server_clause(PoolName, Index, Line + Index) ||
        Index <- lists:seq(0, PoolSize)],
    server_clauses(T, [ServerClauses | Acc], Line + PoolSize).

server_clause_body(PoolName, Index, Line) ->
    [{atom, Line, server_atom(PoolName, Index)}].

server_clause_ps(PoolName, Index, Line) ->
    [{atom, Line, PoolName}, {integer, Line, Index}].
