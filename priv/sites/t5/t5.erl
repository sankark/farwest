-module(t5).
-export([start/0]).
start() ->
    site_sup:start_link(t5).

