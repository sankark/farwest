%% @author Administrator
%% @doc @todo Add description to site_template.


-module(site_template).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/1]).


%% ====================================================================
%% Internal functions
%% ====================================================================

start(SiteName)->
	site_sup:start_link(SiteName).