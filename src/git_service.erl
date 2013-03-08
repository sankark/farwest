%% @author Administrator
%% @doc @todo Add description to git_service.


-module(git_service).

%% ====================================================================
%% API functions
%% ====================================================================
-export([get_repositories/0,clone_repo/1,get_status/1]).
-include("constants.hrl").
-define(BUCKET,<<"git">>).
-define(CLONE_URL,<<"/orion/gitapi/clone">>).
-define(FILE_URL,<<"/orion/file">>).
-define(STATUS_URL,<<"/orion/gitapi/status">>).
-define(COMMIT_URL,<<"/orion/gitapi/commit">>).
-define(CHANGED,<<"Changed">>).
-define(ADDED,<<"Added">>).
-define(REMOVED,<<"Removed">>).

%% ====================================================================
%% Internal functions
%% ====================================================================
get_repositories()->
	[{?KEY_CHILDREN, get_children()}].

get_children()->
	{ok,Keys} = fw_data_server:get_keys(?BUCKET),
	F=fun(Key)->
			Content = get_repo(Key),
			[{?KEY_TYPE,<<"Clone">>}|jsx:decode(Content)]
	end,
	[F(Key)||Key<-Keys].

get_repo(Key) ->
    {ok,Content} = fw_data_server:get_value(?BUCKET, Key),
    Content.
clone_repo(Req)->
	Body = util:get_body_from_req(Req),
	Term = util:json_to_term(Body),
	URL = proplists:get_value(<<"GitUrl">>, Term),
	P= proplists:get_value(<<"Path">>, Term),
	Name = proplists:get_value(<<"Name">>, Term),
	Path = case P of
		undefined -> Name;
		_ -> filename:join(P,Name)
	end,
	RepoURL = binary_to_list(URL),
	RepoPath = filename:join([util:get_priv(),Path]),
	io:format("before clon"),
	{ok, _Resp} = git:clone(RepoURL, RepoPath),
	io:format("after clon"),
	Location = ?CLONE_URL,
	FileLocation = filename:join([?FILE_URL,Path]),
	CloneLocation = filename:join([?CLONE_URL,remove_slash(FileLocation)]),
	CommitLocation = filename:join([?COMMIT_URL,remove_slash(FileLocation)]),
	StatusLocation =filename:join([?STATUS_URL,remove_slash(FileLocation)]),
	UserData = [{?KEY_LOCATION,Location},{?KEY_NAME,Name},
				{?CLONE_LOCATION,CloneLocation},{?GIT_URL,URL}
			   ,{?GIT_STATUS_LOCATION,StatusLocation},
				{?GIT_COMMIT_LOCATION,CommitLocation},{?GIT_CONTENT_LOCATION,FileLocation}],
	fw_data_server:set_data(?BUCKET, Name , <<"">>, jsx:encode(UserData), <<"">>),
	UserData.

get_status([_,_,_|Rest])  ->
	Suffix = filename:join(Rest),
	RepoPath = filename:join([util:get_priv(),binary_to_list(remove_slash(Suffix))]),
	%%io:format("~p",[get_changed_files(RepoPath)]),
	Changed = get_changed_files(RepoPath,Suffix),
	Repo = jsx:decode(get_repo(filename:basename(Suffix))),
	lists:append(Changed,Repo).

get_changed_files(Repo,Suffix)->
	Result = git:status_changed_files(Repo),
	build_status_response(Result,Suffix).

build_status_response(Result,Suffix)->
	ModFun = fun(Elem)-> case Elem of
											{modified,_}-> true;
											_ -> false
										end
			 end,
	NewFun = fun(Elem)-> case Elem of
											{untracked,_}-> true;
											_ -> false
										end
			 end,
	RemFun = fun(Elem)-> case Elem of
											{deleted,_}-> true;
											_ -> false
										end
			 end,	
	Modified = lists:filter(ModFun,Result),
	Added = lists:filter(NewFun,Result),
	Removed = lists:filter(RemFun,Result),
	
	Fun=fun(List)->
		lists:foldl(fun({_,"./"++Abs},Acc)-> 
						[[{?KEY_NAME,lb(filename:basename(Abs))},
						 {?KEY_LOCATION,lb(filename:join([bl(?FILE_URL),bl(Suffix),Abs]))},{?KEY_PATH,lb(Abs)}]|Acc] end, 
				[], List)
		end,
	[{<<"RepositoryState">>,<<"SAFE">>},{<<"Untracked">>,[]},{<<"Clone">>,<<"Test">>},{?CHANGED,Fun(Modified)},{?ADDED,Fun(Added)},{?REMOVED,Fun(Removed)},{?KEY_TYPE,<<"Status">>}].
	
lb(L)->
	list_to_binary(L).
bl(B)->
	binary_to_list(B).
remove_slash(Loc) when is_binary(Loc) ->
	list_to_binary(remove_slash(binary_to_list(Loc)));
remove_slash(Loc)->
case Loc of
"" -> "";
"/" ++ P2 -> P2;
_ -> Loc
    end.