%% records
-record(cast, {
    client         :: shackle:client(),
    pid            :: undefined | pid(),
    request_id     :: shackle:request_id(),
    timeout        :: timeout(),
    timestamp      :: integer()  %% erlang:monotonic_time(microsecond)
}).

-record(reconnect_state, {
    current :: undefined | shackle:time(),
    max     :: shackle:time() | infinity,
    min     :: shackle:time()
}).
