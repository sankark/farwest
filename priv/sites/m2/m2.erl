-module(m2).
-export([start/0]).
start() ->
    site_sup:start_link(m2).

