-include_lib("eunit/include/eunit.hrl").

-define(PORT, 43215).
-define(T, fun (Test) -> test(Test) end).
-define(TCP_OPTIONS, [
    binary,
    {active, false},
    {reuseaddr, true}
]).
