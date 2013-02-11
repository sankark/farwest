-module(ezwebframe_demos).
-export([start/0]).
-export([dispatch/1]).

%% This demo assumes that all your code
%% the code paths to ezwebframe and simple_demo have beed setup 

start() ->
    %% compile all the code in the subdirectories
    %% and set paths appropriately
    io:format("a simple_demo....~n"),
    Port = 1456,
    io:format("Load the page http://localhost:~p/ in your browser~n",[Port]),
    ezwebframe:start_link(fun ?MODULE:dispatch/1, Port).

%% dispatch maps names in the HTML onto fixed paths 

dispatch(F) ->
io:format("inside dispatch"),
    F1 = dispatch1(F),
    io:format("ezwebframe_demos::dispatch ~s => ~s~n",[F,F1]),
    F1.

dispatch1("/ezwebframe/" ++ F) ->
    Dir = dir(2, code:which(ezwebframe)) ++ "/priv/",
    Dir ++ F;
dispatch1("/" ++ F) ->
    Dir = dir(2,code:which(?MODULE)) ++ "/",
    Dir ++ F.

dir(0, F) -> F;
dir(K, F) -> dir(K-1, filename:dirname(F)).

