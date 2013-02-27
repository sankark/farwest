-module(site_handler).
-export([init/3]).
-export([rest_init/2]).
-export([allowed_methods/2]).
-export([is_authorized/2]).
-export([charsets_provided/2]).
-export([content_types_accepted/2]).
-export([from_json/2]).
-export([from_form/2]).
-export([to_html/2]).
-export([to_json/2]).
-export([content_types_provided/2,to_html_for_site/2,to_html_for_json/2]).

-record(state, {
				auth = undefined,
				userid = undefined,
				collection = undefined
			   }).

init(_, _, _) ->
	{upgrade, protocol, cowboy_rest}.

rest_init(Req, Opts) ->
	State = case lists:keyfind(auth, 1, Opts) of
				{auth, AuthOpts} -> #state{auth=AuthOpts};
				false -> #state{}
			end,
	{Name, _Req2} = cowboy_req:path_info(Req),
	{ok, Req, State#state{collection=Name =:= []}}.


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

content_types_provided(Req, State=#state{collection=true}) ->
	{[
	  {<<"text/html">>, to_html},
	  {<<"application/json;charset=UTF-8">>, to_json}
	 ], Req, State};

content_types_provided(Req, State=#state{collection=false}) ->
	{[
	  {<<"text/html">>, to_html_for_site},
	  {<<"application/json;charset=UTF-8">>, to_html_for_json}
	 ], Req, State}.

content_types_accepted(Req, State) ->
	{[{<<"application/json;charset=utf-8">>, from_json},
	  {<<"application/x-www-form-urlencoded;charset=UTF-8">>, from_form}
	 ], Req, State}.


from_json(Req, State) ->
	JSON = util:get_body_from_req(Req),
	JSONTerm = jsx:decode(JSON),
	Response = case site_service:create_site(JSONTerm) of
				   {ok, Resp}-> Resp;
				   {error, Msg} -> {error, Msg}
			   end,
	%% @todo
	Req2 = util:set_json_resp(jsx:encode(Response),Req),
	{true, Req2, State}.

from_form(Req, State) ->
	%% @todo
	{true, Req, State}.

to_html(Req,State)->
	Sites = site_service:get_sites(),
	Body = jsx:encode(Sites),
	Req2 = util:set_resp_header(<<"content-type">>,<<"application/json">>,Req),
	%% @todo
	{Body,Req2,State}.

to_json(Req,State)->
	%% @todo
	{<<"Body">>,Req,State}.



to_html_for_site(Req,State)->
	{[Name], _Req2} = cowboy_req:path_info(Req),
	{ok,Site} = site_service:get_site_details(Name),
	Req2 = util:set_resp_header(<<"content-type">>,<<"application/json">>,Req),
	%% @todo
	{Site,Req2,State}.

to_html_for_json(Req,State)->
	%% @todo
	{<<"Body">>,Req,State}.
