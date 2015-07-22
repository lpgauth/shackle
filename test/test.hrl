-include_lib("eunit/include/eunit.hrl").

-define(CLIENT, arithmetic_client).
-define(POOL_NAME, arithmetic).
-define(PORT, 43215).
-define(T, fun (Test) -> test(Test) end).
