%% @author Administrator
%% @doc @todo Add description to sync_service.


-module(sync_service).

%% ====================================================================
%% API functions
%% ====================================================================
-export([get_compile_dir/0]).



%% ====================================================================
%% Internal functions
%% ====================================================================

get_compile_dir()->
	{ok,[{"c:/tmp/farwest/priv/resources",[report]}]}.
