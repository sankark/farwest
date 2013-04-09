%% @author Administrator
%% @doc @todo Add description to git_service.


-module(git_service).

%% ====================================================================
%% API functions
%% ====================================================================
-export([get_repositories/0,get_repository/2,clone_repo/1,get_status/2,commit_repo/2]).
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
get_repository(Path,_Req)->
	Commits = get_commit_history(Path),
	Children = [{?KEY_CHILDREN, [lists:append(get_repo2(lb(Path)),Commits)]}],
	
	lists:append(Children,Commits).

get_children()->
	{ok,Keys} = fw_data_server:get_keys(?BUCKET),
	F=fun(Key)->
			get_repo2(Key)
			end,
	[F(Key)||Key<-Keys].

get_repo2(Key) ->
			 Content = get_repo(Key),
    [{?KEY_TYPE,<<"Clone">>}|jsx:decode(Content)].

get_repo(Key) ->
    {ok,Content} = case fw_data_server:get_value(?BUCKET, Key) of
					   {ok, Any}->{ok, Any};
					   {error,notfound}-> {ok, []}
				   end,
    Content.
clone_repo(Req)->
	Body = util:get_body_from_req(Req),
	Term = util:json_to_term(Body),
	URL = proplists:get_value(<<"GitUrl">>, Term),
	P= proplists:get_value(<<"Path">>, Term),
	Path = case P of
		undefined -> proplists:get_value(<<"Name">>, Term);
		_ -> RepoName = filename:basename(URL,".git"),
			 <<"/orion/file/", Suffix/binary>> = P,
			filename:join([Suffix,RepoName])
	end,
	Name = Path,
	io:format("Path ****************** ~p",[Path]),
	RepoURL = binary_to_list(URL),
	RepoPath = filename:join([util:get_priv(),bl(Path)]),
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

commit_repo(Path,Req)->
	Body = util:get_body_from_req(Req),
	Term = util:json_to_term(Body),
	RepoPath = filename:join([util:get_priv(),Path]),
	Msg = proplists:get_value(<<"Message">>, Term),
	git:commit(RepoPath, bl(Msg)),
	fw_data_server:set_data(<<"git_commit_history">>, Path , <<"">>, jsx:encode(Term), <<"">>),	
	get_commit_history(Path).


get_commit_history(RepoPath) ->
	Chidren = case get_commits(RepoPath) of
					 [] -> [];
					 Any -> jsx:decode(Any)
				 end,
	io:format("Chidler ********~p",[Chidren]),
	Location = [{?KEY_LOCATION,filename:join([<<"/orion/gitapi/push">>,RepoPath])}],
	Commits = [{?KEY_CHILDREN ,[util:key_merge(Location, Chidren)]}],
	[{<<"Commits">>,Commits}].

get_commits(RepoPath)->
	{ok,Content} = case fw_data_server:get_value(<<"git_commit_history">>, RepoPath) of
					   {ok, Any} -> {ok, Any};
				   {error,notfound}-> {ok,[]}
				   end,
	Content.
get_status(Path, _Req)  ->
	RepoPath = filename:join([util:get_priv(),Path]),
	%%io:format("~p",[get_changed_files(RepoPath)]),
	Changed = get_changed_files(RepoPath),
	CommitHist= get_commit_history(Path),
	Repo = jsx:decode(get_repo(lb(filename:basename(Path)))),
	lists:append(Changed,Repo).

get_changed_files(Repo)->
	Result = git:status_changed_files(Repo),
	build_status_response(Result,lb(filename:basename(Repo))).

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