-module(shackle_client).
-include("shackle_internal.hrl").

%% callbacks
-callback after_connect(Socket :: inet:socket(), State :: term()) ->
    {ok, State :: term()} |
    {error, Reason :: term(), State :: term()}.

-callback handle_cast(Cast :: term(), State :: term()) ->
    {ok, RequestId :: external_request_id(), Data :: iodata(), State :: term()}.

-callback handle_data(Data :: binary(), State :: term()) ->
    {ok, [{RequestId :: external_request_id(), Reply :: term()}], State :: term()}.

-callback options() -> {ok, Options :: client_options()}.

-callback process_timings(Cast :: term(), [non_neg_integer()]) -> ok.

-callback terminate(State :: term()) -> ok.
