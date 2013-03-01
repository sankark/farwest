-module(dsfd).
-export([start/0]).
start() ->
    site_sup:start_link(dsfd).

