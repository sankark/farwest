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
	try get_src_dirs() of
		{ok, Paths} -> {ok, Paths}
	catch
		exit:_Error -> {ok,[]}
	end.
		
		

get_src_dirs() ->
	   %%io:format("iside "),
	   SiteNames = site_service:get_site_names(),
	   SiteAbs = [site_service:get_site_abs(Site)||Site <- SiteNames],
		  SrcDirs = ["modules","resources",""],
	   Fun = fun(Dir) -> [{filename:join([Site,Dir]),get_opts(Site)}||Site <- SiteAbs]
	         end,
	   Paths = lists:flatten([Fun(Dir)||Dir <- SrcDirs]),
    %%io:format("Paths ~p",[Paths]),
    {ok,Paths}.
get_opts(Site)->
	[{outdir, filename:join([Site,"ebin"])}].