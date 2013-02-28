%% @author Administrator
%% @doc @todo Add description to site_service.


-module(site_service).
-include("constants.hrl").
%% ====================================================================
%% API functions
%% ====================================================================
-export([create_site/1,start_site/1,stop_site/1,get_sites/0,get_site_details/1,parse_request/2]).
-define(BUCKET,<<"sites">>).

create_site(JsonTerm)->
	SiteName = util:get_value_from_proplist(?KEY_NAME , JsonTerm),
	case create_site_structure(SiteName) of
		ok -> 	{ok,compose_response(JsonTerm,SiteName)};
		{error, Any} -> {error, Any}
	end.
start_site(SiteName)->
	[].
stop_site(SiteName)->
	[].

parse_request(Site,JSONTerm)->
	SiteStatus = proplists:get_value(?SITE_HOST_STATUS, JSONTerm,[]),
	%%io:format("Site Status ~p~n",[SiteStatus]),
	case SiteStatus of
		[] -> get_site_details(Site);
		[{_,<<"started">>}] -> start_site(Site),
						 {ok,Details}= get_site_details(Site),
						 DetTerm = jsx:decode(Details),
						 Result = merge(JSONTerm,DetTerm),
						 store_data(Site,Result),
						 {ok, Result}
						 
	end.

get_sites()->
	{ok,Keys} = fw_data_server:get_keys(<<"sites">>),
	Sites = case Keys of
		[] -> [];
		_ -> [jsx:decode(get_value(K))||K<-Keys]
			end,
	[{?SITE_CONFIG,Sites}].


get_value(K)->
	{ok,Value} = fw_data_server:get_value(?BUCKET, K),
	Value.
get_site_details(SiteName)->
	Site = binary_to_list(SiteName),
	fw_data_server:get_value(?BUCKET, Site).

%% ====================================================================
%% Internal functions
%% ====================================================================

merge(PL1, PL2)->
	util:key_merge(lists:ukeysort(1,PL1), lists:ukeysort(1,PL2)).
compose_response(JsonTerm,SiteName)->
	Site = binary_to_list(SiteName),
	Location = list_to_binary("/orion/file/sites/"++Site),
	JsonTerm2 = lists:append([{?SITE_LOCATION,list_to_binary("/orion/site/"++Site)},{?KEY_ID,SiteName},{?KEY_LOCATION,Location}],JsonTerm),
	SiteDetail = update_with_default(JsonTerm2),
	fw_data_server:set_data(?BUCKET, Site, Site, jsx:encode(SiteDetail), Site),
	SiteDetail.
	
store_data(Key,Value)->
	fw_data_server:set_data(?BUCKET, Key, <<"">>, jsx:encode(Value), <<"">>).
get_defaults(Key)->
	fw_data_server:get_value(<<"defaults">>,Key).

update_with_default(JsonTerm) ->
	 {ok,Default} = get_defaults(<<"site">>),
	  Def2 = jsx:decode(Default),
    util:key_merge(lists:ukeysort(1,JsonTerm), lists:ukeysort(1,Def2)).

create_site_structure(SiteName)->
	case check_if_exist(SiteName) of
		true -> {error, <<"Site Already Exists">>};
		false -> copy_structure(SiteName)
	end.
copy_structure(SiteName)->
	SkelDir = fw_config:get(skeleton_dir),
	NewSiteDir = get_site_abs(SiteName),
	util:recursive_copy(SkelDir,NewSiteDir).
check_if_exist(SiteName)->
	case fw_data_server:get_value(?BUCKET, SiteName) of
		{ok, _} -> true;
		{error,notfound} -> false
	end.
get_site_abs(SiteName) ->
	   SitesDir = fw_config:get(sites_dir),
    filename:join([SitesDir,binary_to_list(SiteName)]).