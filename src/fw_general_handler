%% /farwest/templates/[name]
-module(fw_signup_handler).

-export([init/3]).
-export([rest_init/2]).
-export([allowed_methods/2]).
-export([is_authorized/2]).
-export([content_types_provided/2]).
-export([charsets_provided/2]).
-export([resource_exists/2]).
-export([content_types_accepted/2]).
-export([to_html/2]).
-export([to_json/2]).
-export([from_json/2]).
-export([from_form/2]).
%% @todo -export([template_from_json/2]).
-export([delete_resource/2]).
-export([delete_completed/2]).

-record(state, {
	auth = undefined,
	template = undefined :: boolean(),
	userid = undefined
}).

init(_, _, _) ->
	{upgrade, protocol, cowboy_rest}.

rest_init(Req, Opts) ->
	State = case lists:keyfind(auth, 1, Opts) of
		{auth, AuthOpts} -> #state{auth=AuthOpts};
		false -> #state{}
	end,
	{template, Template} = lists:keyfind(template, 1, Opts),
	Module = list_to_existing_atom(atom_to_list(Template) ++ "_dtl"),
	{ok, Req, State#state{template=Module}}.

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

content_types_provided(Req, State) ->
	{[
		{<<"text/html">>, to_html},
		{<<"application/json">>, to_json}
	], Req, State}.

%% Only allow UTF-8.
charsets_provided(Req, State) ->
	{[{<<"utf-8">>, 1000}, {<<"*">>, 0}], Req, State}.

resource_exists(Req, State) ->
	{true, Req, State}.

content_types_accepted(Req, State) ->
	{[
		{<<"application/x-www-form-urlencoded;charset=UTF-8">>,
			from_form},
		{<<"application/json">>, from_json}
	], Req, State}.



to_html(Req, State=#state{template=Module}) ->
	{ok,Body} = Module:render([]),
	{Body, Req, State}.

to_json(Req, State) ->
	%% @todo
	Body = <<"{}">>,
	{Body, Req, State}.

from_json(Req, State) ->
	%% @todo
	{true, Req, State}.

%% @todo Later just send JSON from form directly.
%set_data(Bucket, Key, EditBy, UserData, Comments) 
from_form(Req, State) ->
	{ok, Values, Req3} = cowboy_req:body_qs(Req),
	io:format("Values ~p~n",[jsx:encode(Values)]),
	{<<"user_name">>, UserName} = lists:keyfind(<<"user_name">>, 1, Values),
	fw_userdata_server:set_data(<<"userdata_profiles">>,UserName,UserName,jsx:encode(Values),<<"test">>),
	{<<"password">>, _Password} = lists:keyfind(<<"password">>, 1, Values),
	io:format("user_name ~p~n",[UserName]),
			{true, Req3, State}.
		

%% @todo template_from_json

delete_resource(Req, State) ->
	%% @todo
	io:format("delete_resource ~p~n", [Req]),
	{true, Req, State}.

delete_completed(Req, State) ->
	%% @todo
	{true, Req, State}.
