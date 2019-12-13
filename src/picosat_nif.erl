-module(picosat_nif).
-on_load(init/0).

-export([solve/1]).

init() ->
    PrivDir = code:priv_dir(picosat_elixir),
    Path = filename:join(PrivDir, "picosat_nif"),
    ok = erlang:load_nif(Path, 0).


solve(_) ->
    exit(unable_to_load_picosat_nif).
