-module(arithmetic_protocol).

-export([
    opcode/1,
    parse_replies/1,
    parse_requests/1,
    request/4,
    request_id/1
]).

-define(MAX_REQUEST_ID, 256).

%% public
opcode(add) -> 1;
opcode(multiply) -> 2.

parse_replies(Data) ->
    parse_replies(Data, []).

parse_requests(Data) ->
    parse_requests(Data, []).

request(RequestId, Operation, A, B) ->
    <<RequestId:8/integer, (opcode(Operation)), A:8/integer, B:8/integer>>.

request_id(RequestCounter) ->
    RequestCounter rem ?MAX_REQUEST_ID.

%% private
parse_replies(<<ReqId:8/integer, A:16/integer, Rest/binary>>, Acc) ->
    parse_replies(Rest, [{ReqId, A} | Acc]);
parse_replies(Buffer, Acc) ->
    {Acc, Buffer}.

parse_requests(<<"INIT", Rest/binary>>, Acc) ->
    parse_requests(Rest, [<<"OK">> | Acc]);
parse_requests(<<ReqId:8/integer, 1, A:8/integer, B:8/integer,
    Rest/binary>>, Acc) ->

    parse_requests(Rest, [<<ReqId:8/integer, (A + B):16/integer>> | Acc]);
parse_requests(<<ReqId:8/integer, 2, A:8/integer, B:8/integer,
    Rest/binary>>, Acc) ->

    parse_requests(Rest, [<<ReqId:8/integer, (A * B):16/integer>> | Acc]);
parse_requests(Buffer, Acc) ->
    {Acc, Buffer}.
