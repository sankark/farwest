%% @author Administrator
%% @doc @todo Add description to util.


-module(util).

%% ====================================================================
%% API functions
%% ====================================================================
-export([absolute_path/1,add_site_path/1]).

-export([get_path/1,set_resp_header/3,key_merge/2,set_json_resp/2,get_value_from_proplist/2,json_to_term/1,term_to_json/1,set_resp_header/3,set_resp_body/2,get_body_from_req/1,recursive_copy/2,get_site_home/1,start_slave/1,stop_slave/1]).

-export([get_priv/0,path_info/1]).



%% ====================================================================
%% Internal functions
%% ====================================================================

absolute_path(Path)->
	filename:join([code:lib_dir(farwest), "priv",Path]).

add_site_path(SitePath)->
	code:add_pathz(SitePath).

get_value_from_proplist(Key,JsonReq) -> proplists:get_value(Key, JsonReq).

json_to_term(Body) -> jsx:decode(Body).

term_to_json(Body) -> jsx:encode(Body).

set_resp_header(K,V,Req)->
	cowboy_req:set_resp_header(K, V, Req).

set_resp_body(Body,Req)->
	cowboy_req:set_resp_body(Body,Req).

set_json_resp(Body,Req)->
	Req2 = set_resp_header(<<"content-type">>,<<"application/json">>,Req),
	set_resp_body(Body,Req2).

get_body_from_req(Req) ->
	%% @todo
	{ok, [{Body,_}], _Req2} = cowboy_req:body_qs(Req),
	Body.

key_merge(NewList,OldList) ->
	lists:ukeymerge(1,NewList,OldList).


%% Recursively copy directories
-spec recursive_copy(list(), list()) -> ok.                            
recursive_copy(From, To) ->
	{ok, Files} = file:list_dir(From),
	[ok = rec_copy(From, To, X) || X <- Files],
	ok.

start_slave(Site)->
	SiteBin = filename:join([".","priv","sites",Site,"ebin"]),
	slave:start(list_to_atom(net_adm:localhost()),Site,"-setcookie cookie -pa ./deps/cowboy/ebin ./deps/ranch/ebin ./ebin "++SiteBin),
	Node = list_to_atom(Site++"@"++net_adm:localhost()),
	io:format("started"),
	rpc:call(Node, list_to_atom(Site), start, []).

stop_slave(Site)->
	Node = list_to_atom(Site++"@"++net_adm:localhost()),
	slave:stop(Node).

rec_copy(From, To, File) ->
	
	NewFrom = filename:join([From, File]),
	NewTo   = filename:join([To, File]),
	io:format("File ~p   ~p    ~n",[NewFrom,NewTo]),
	case filelib:is_dir(NewFrom) of
		true  ->
			ok = make_dir(NewTo),
			recursive_copy(NewFrom, NewTo);
		
		false ->
			case filelib:is_file(NewFrom) of                
				true  ->
					ok = filelib:ensure_dir(NewTo),
					{ok, _} = file:copy(NewFrom, NewTo),
					ok;
				false ->
					ok            
			end
	end.

make_dir(Dir)->
	case file:make_dir(Dir) of
		{error,eexist} -> ok;
		_  -> ok
	end.

get_site_home(SiteName) when is_list(SiteName) ->
	get_site_home(list_to_atom(SiteName));
get_site_home(SiteName) ->
	BeamPath = code:which(SiteName),
	SplitPath = filename:split(BeamPath),
	SplitPath2 = lists:sublist(SplitPath,length(SplitPath)-2),
	SiteHome = filename:join(SplitPath2),
	io:format("Site dir ~p",[SiteHome]),
	SiteHome.

get_path(Req) ->
	%%io:format("path info ~p",[cowboy_req:path_info(Req)]),
	{PI,_} = cowboy_req:path_info(Req),
	binary_to_list(filename:join(PI)).
	
get_priv()->
	filename:join([code:lib_dir(farwest), "priv"]).

path_info(Req) ->
	%%io:format("path info ~p",[cowboy_req:path_info(Req)]),
	{PI,_} = cowboy_req:path_info(Req),
	PI.

