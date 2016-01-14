-module(shackle_client).
-include("shackle_internal.hrl").

% client options (e.g. ip, port)
-callback options() -> {ok, Options :: client_options()}.

% init state
-callback init() -> {ok, State :: term()}.

% setup socket (e.g. set default keyspace)
-callback setup(Socket :: inet:socket(), State :: term()) ->
    {ok, State :: term()} |
    {error, Reason :: term(), State :: term()}.

% serialize requests into protocol
-callback handle_request(Request :: term(), State :: term()) ->
    {ok, RequestId :: external_request_id(), Data :: iodata(), State :: term()}.

% deserialize protocol into responses
-callback handle_data(Data :: binary(), State :: term()) ->
    {ok, [Response :: response()], State :: term()}.

% terminate state
-callback terminate(State :: term()) -> ok.
