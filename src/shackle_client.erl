-module(shackle_client).
-include("shackle_internal.hrl").

%% callbacks
-callback after_connect(Socket :: inet:socket(), State :: term()) ->
    {ok, State :: term()} |
    {error, Reason :: term(), State :: term()}.

-callback handle_data(Data :: binary(), State :: term()) ->
    {ok, [Response :: response()], State :: term()}.

-callback handle_request(Request :: term(), State :: term()) ->
    {ok, RequestId :: external_request_id(), Data :: iodata(), State :: term()}.

-callback handle_timing(Request :: term(), Timing :: [non_neg_integer()]) -> ok.

-callback init() -> {ok, State :: term()}.

-callback options() -> {ok, Options :: client_options()}.

-callback terminate(State :: term()) -> ok.
