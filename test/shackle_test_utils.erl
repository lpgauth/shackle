-module(shackle_test_utils).

-export([
    preload_modules/0
]).

%% public
preload_modules() ->
    Filenames = filelib:wildcard("_build/default/lib/*/ebin/*.beam"),
    Rootnames = [filename:rootname(Filename, ".beam") || Filename <- Filenames],
    lists:foreach(fun code:load_abs/1, Rootnames).
