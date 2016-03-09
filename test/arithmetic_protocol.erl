-module(arithmetic_protocol).
-include("shackle_internal.hrl").

-export([
    opcode/1,
    parse_replies/1,
    parse_requests/1,
    request/4,
    request_id/1
]).

-define(MAX_REQUEST_ID, 256).

-type operation() :: add | multiply.
-type tiny_int() :: 0..255.

%% public
-spec opcode(operation()) -> 1..2.

opcode(add) -> 1;
opcode(multiply) -> 2.

-spec parse_replies(binary()) -> {[response()], binary()}.

parse_replies(Data) ->
    parse_replies(Data, []).

-spec parse_requests(binary()) -> {[binary()], binary()}.

parse_requests(Data) ->
    parse_requests(Data, []).

-spec request(tiny_int(), operation(), tiny_int(), tiny_int()) -> binary().

request(RequestId, Operation, A, B) ->
    <<RequestId:8/integer, (opcode(Operation)), A:8/integer, B:8/integer>>.

-spec request_id(non_neg_integer()) -> tiny_int().

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
