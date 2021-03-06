%% @author Administrator
%% @doc @todo Add description to code_gen.


-module(code_gen).
-compile({parse_transform, parse_trans_codegen}).
-export([gen_start_function/1,gen_site_module/1]).


%% ====================================================================
%% API functions
%% ====================================================================
-pt_pp_src(true).

gen_start_function(Site) ->
codegen:gen_function(start, fun() -> application:start(ranch),
									 application:start(crypto),
									 application:start(cowboy),
									 site_sup:start_link({'$var',Site}) end).

gen_site_module(SiteName)->
codegen:gen_module({'$var',SiteName}, [{start,0}], [{start,fun() -> 
																  application:start(ranch),
									 application:start(crypto),
									 application:start(cowboy),
									 site_sup:start_link({'$var',SiteName}),
									 IP = site_config:get(ip),
									Port = site_config:get(http_port),
									 {IP,Port}
													 end}]).