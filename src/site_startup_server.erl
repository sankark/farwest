-module(site_startup_server).

%% API.
-export([start_link/1]).
-export([stop/0]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {
	cowboy_pid = undefined :: pid()
}).

start_link(SiteName) ->
	case gen_server:start_link({local, ?MODULE}, ?MODULE, SiteName, []) of
		{ok, Pid} -> {ok,Pid};
		{error, {already_started, Pid}} -> {ok, Pid}
	end.

stop() ->
	gen_server:call(?MODULE, stop).


%% gen_server.

init(Site) ->
	SiteName = atom_to_list(Site),
	process_flag(trap_exit, true),
	Port = site_config:get(http_port, 8085),
	SSLPort = site_config:get(https_port, 8443),
	Certfile = site_config:get(https_cert),
	CACertfile = site_config:get(https_cacert),
	SitesHome = util:get_site_home(SiteName),
	IP = site_config:get(ip, {127,0,0,1}),
	{ok, Routes} = file:consult(filename:join([SitesHome, "dispatch","dispatch"])),
	%% HTTP.
	io:format("routes ~p",[Routes]),
	HttpSite = list_to_atom(SiteName++"_http"),
	HttpsSite = list_to_atom(SiteName++"_https"),
	{ok, Pid} = cowboy:start_http(HttpSite, 100,
		[{ip,IP},{port, Port}], [{dispatch, Routes}]
	),
	{ok, #state{cowboy_pid = Pid}}.


handle_call(stop, _, State) ->
	{stop, {info, died}, dead, State};

handle_call(_, _, State) ->
	{reply, ignore, State}.

handle_cast(_, State) ->
	{noreply, State}.

handle_info(_, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% Internal.

