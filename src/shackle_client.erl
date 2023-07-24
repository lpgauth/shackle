-module(shackle_client).

-optional_callbacks([handle_timeout/2]).

-callback init(Options :: term()) ->
    {ok, State :: term()} |
    {error, Reason :: term()}.

-callback setup(Socket :: inet:socket(), State :: term()) ->
    {ok, State :: term()} |
    {error, Reason :: term(), State :: term()}.

-callback handle_request(Request :: term(), State :: term()) ->
    {ok, RequestId :: shackle:external_request_id(), Data :: iodata(), State :: term()}.

-callback handle_data(Data :: binary(), State :: term()) ->
    {ok, [Response :: shackle:response()], State :: term()} |
    {error,  Reason :: term(), State :: term()}.

-callback handle_timeout(RequestId :: shackle:external_request_id(), State :: term()) ->
    {ok, Response :: shackle:response(), State :: term()} |
    {error,  Reason :: term(), State :: term()}.

-callback terminate(State :: term()) ->
    ok.

-type option() :: {address, shackle:inet_address()} |
                  {init_options, shackle_server:init_options()} |
                  {ip, shackle:inet_address()} |
                  {port, shackle:inet_port()} |
                  {protocol, shackle:protocol()} |
                  {reconnect, boolean()} |
                  {reconnect_time_max, shackle:time() | infinity} |
                  {reconnect_time_min, shackle:time()} |
                  {socket_options, shackle:socket_options()}.
-type options() :: [option()].

-export_type([options/0]).
