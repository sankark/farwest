%% Copyright (c) 2012, Loïc Hoguin <essen@ninenines.eu>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

%% @private
-module(farwest_app).
-behaviour(application).

%% API.
-export([start/2]).
-export([stop/1]).

%% API.

start(_, _) ->
	set_path(),
	Port = int_env(http_port, 8085),
	SSLPort = int_env(https_port, 8443),
	Certfile = path_env(https_cert),
	CACertfile = path_env(https_cacert),
	{ok, Routes} = file:consult(path_env(routes_file)),
	%% HTTP.
	io:format("routes ~p",[Routes]),
	{ok, _} = cowboy:start_http(farwest_http, 100,
		[{port, Port}], [{dispatch, Routes}]
	),
	lager:info("Farwest listening on port ~p~n", [Port]),
	{ok, _} = cowboy:start_https(farwest_https, 100,
		[{port, SSLPort}, {certfile, Certfile},
			{cacertfile, CACertfile}, {verify, verify_peer}],
		[{dispatch, Routes}]
	),
	lager:info("Farwest securely listening on port ~p~n", [SSLPort]),
	farwest_sup:start_link().

stop(_) ->
	ok.

%% Internal.

int_env(Key, Default) ->
	case application:get_env(farwest, Key) of
		{ok, Value} when is_integer(Value) ->
			Value;
		undefined ->
			Default
	end.

path_env(Key) ->
	case application:get_env(farwest, Key) of
		{ok, {priv_dir, App, Path}} ->
			code:priv_dir(App) ++ "/" ++ Path;
		{ok, Path} ->
			Path
	end.
set_path() ->
	P = code:all_loaded(),
	Path = filename:dirname(filename:dirname(proplists:get_value(?MODULE, P))),
	code:add_pathz(Path),
	application:set_env(farwest, lib_dir, Path),
	application:set_env(farwest, https_cert, Path ++ "/priv/cert/sample.crt"),
	application:set_env(farwest, https_cacert,Path ++ "/priv/cert/sample.cert"),
	application:set_env(farwest, userfiles_dir,Path ++ "/priv/userfiles_dir"),
	application:set_env(farwest, config_file,Path ++ "/priv/config"),
	application:set_env(farwest, routes_file,Path ++ "/priv/dispatch/dispatch").