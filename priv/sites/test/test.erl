-module(test).
-export([start/0]).
start() ->
    application:start(ranch),
    application:start(crypto),
    application:start(cowboy),
    site_sup:start_link(test),
    IP = site_config:get(ip),
    Port = site_config:get(http_port),
    {IP,Port}.

