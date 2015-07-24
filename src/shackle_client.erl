-module(shackle_client).
-include("shackle.hrl").

%% callbacks
-callback opts() -> {ok, Opts :: client_opts()}.

-callback after_connect(Socket :: inet:socket(), State :: term()) ->
    {ok, State :: term()} |
    {error, Reason :: term(), State :: term()}.

-callback handle_cast(Request :: term(), State :: term()) ->
    {ok, RequestId :: term(), Data :: iodata(), State :: term()}.

-callback handle_data(Data :: binary(), State :: term()) ->
    {ok, [{RequestId :: term(), Reply :: term()}], State :: term()}.

-callback terminate(State :: term()) -> ok.
