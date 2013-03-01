-module(test3).
-export([start/0]).
start() ->
    site_sup:start_link(test3).

