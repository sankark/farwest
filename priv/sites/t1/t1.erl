-module(t1).
-export([start/0]).
start() ->
    site_sup:start_link(t1).

