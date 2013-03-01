-module(test).
-export([start/0]).
start() ->
    site_sup:start_link(test).

