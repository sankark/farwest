%% @author Administrator
%% @doc @todo Add description to site_manager.


-module(site_manager).

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).



%% ====================================================================
%% Internal functions
%% ====================================================================

start_site(SiteName)->
	SitesDir = site_service:get_site_abs(SiteName),
	Port = int_env(http_port, 8085),
	SSLPort = int_env(https_port, 8443),
	Certfile = path_env(https_cert),
	CACertfile = path_env(https_cacert),
	IP = get_next_loopback_ip()
	{ok, Routes} = file:consult(path_env(routes_file)),
	%% HTTP.
	io:format("routes ~p",[Routes]),
	{ok, _} = cowboy:start_http(farwest_http, 100,
		[{port, Port}], [{dispatch, Routes}]
	),
	%%lager:info("Farwest listening on port ~p~n", [Port]),
	{ok, _} = cowboy:start_https(farwest_https, 100,
		[{port, SSLPort}, {certfile, Certfile},
			{cacertfile, CACertfile}, {verify, verify_peer}],
		[{dispatch, Routes}]
	),