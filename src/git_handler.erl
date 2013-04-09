-module(git_handler).
-export([init/3]).
-export([rest_init/2]).
-export([allowed_methods/2]).
-export([is_authorized/2]).
-export([charsets_provided/2]).
-export([content_types_accepted/2]).
-export([from_json/2]).
-export([from_form/2]).
-export([to_form/2]).
-export([content_types_provided/2,to_html/2]).


-record(state, {
	auth = undefined,
	userid = undefined
}).

init(_, _, _) ->
	{upgrade, protocol, cowboy_rest}.

rest_init(Req, Opts) ->
	State = case lists:keyfind(auth, 1, Opts) of
		{auth, AuthOpts} -> #state{auth=AuthOpts};
		false -> #state{}
	end,
	{ok, Req, State#state{}}.


allowed_methods(Req, State) ->
	{[<<"GET">>, <<"PUT">>, <<"DELETE">>], Req, State}.

is_authorized(Req, State=#state{auth=undefined}) ->
	{true, Req, State};
is_authorized(Req, State=#state{auth=AuthOpts}) ->
	case fw_auth:authenticate(Req, AuthOpts) of
		{false, Req2} ->
			{{false, fw_auth:methods(AuthOpts)}, Req2, State};
		{UserID, Req2} ->
			{true, Req2, State#state{userid=UserID}}
	end.


%% Only allow UTF-8.
charsets_provided(Req, State) ->
	{[{<<"utf-8">>, 1000}, {<<"*">>, 0}], Req, State}.

content_types_provided(Req, State) ->
	{[
		{<<"text/html">>, to_html},
		{<<"application/json">>, to_json}
	], Req, State}.

content_types_accepted(Req, State) ->
	{[{<<"application/json;charset=UTF-8">>, from_json},
		{<<"application/x-www-form-urlencoded;charset=UTF-8">>, from_form}
	], Req, State}.


from_json(Req, State) ->
	%% @todo
	Path = util:get_path(Req),
	Response = case Path of
		"clone" -> git_service:clone_repo(Req);
		"commit/orion/file/"++Rest -> git_service:commit_repo(Rest,Req);
					_-> []
	end,
	
	%% @todo
	Req2 = util:set_json_resp(util:term_to_json(Response), Req),
	{true, Req2, State}.
	
from_form(Req, State) ->
	Path = util:get_path(Req),
	Response = case Path of
		"clone/orion/file/"++Rest -> git_service:clone_repo(Rest,Req);
		"status/orion/file/"++Rest -> git_service:get_status(Rest,Req);
					_-> []
	end,
	
	%% @todo
	Req2 = util:set_json_resp(util:term_to_json(Response), Req),
	{true, Req2, State}.
	
to_html(Req,State)->
	%% @todo
	Path = util:get_path(Req),
	Repositories =case Path of
		"clone/orion/workspace"++_Rest ->git_service:get_repositories();
		"clone/orion/file/"++Rest ->git_service:get_repository(Rest,Req);
		"status/orion/file/"++Rest -> git_service:get_status(Rest,Req);
		_ -> []
	end,
	JsonResp = util:term_to_json(Repositories),
	Req2 = util:set_json_resp(JsonResp, Req),
	{JsonResp,Req2,State}.
	
to_form(Req,State)->
	%% @todo
	{<<"Body">>,Req,State}.
