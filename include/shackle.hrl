%% records
-record(cast, {
    client         :: shackle:client(),
    pid            :: pid(),
    send_reply     :: boolean(),   %% false when caller didn't provide its PID
    request_id     :: shackle:request_id(),
    request_state  :: term(),
    sema           :: shackle_sema:sema_ref(),
    timeout        :: timeout(),
    timestamp      :: non_neg_integer(),
    timer_ref      :: undefined | reference()
}).

-record(reconnect_state, {
    current :: undefined | shackle:time(),
    max     :: shackle:time() | infinity,
    min     :: shackle:time()
}).
