-module(shackle_client).
-include("shackle_internal.hrl").

-callback options() ->
    {ok, Options :: client_options()}.

-callback init() ->
    {ok, State :: term()}.

-callback setup(Socket :: inet:socket(), State :: term()) ->
    {ok, State :: term()} |
    {error, Reason :: term(), State :: term()}.

-callback handle_request(Request :: term(), State :: term()) ->
    {ok, RequestId :: external_request_id(), Data :: iodata(), State :: term()}.

-callback handle_data(Data :: binary(), State :: term()) ->
    {ok, [Response :: response()], State :: term()}.

-callback terminate(State :: term()) ->
    ok.
