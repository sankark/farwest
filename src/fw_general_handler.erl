%% /farwest/templates/[name]
-module(fw_general_handler).

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
	put_rules :: list(),
	get_rules :: list(),
	data :: list(),
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
	PutRules = proplists:get_value(put_rules, Opts, []),
	GetRules = proplists:get_value(get_rules, Opts, []),
	Module = list_to_existing_atom(atom_to_list(Template) ++ "_dtl"),
	{ok, Req, State#state{template=Module,put_rules=PutRules,get_rules=GetRules}}.

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

resource_exists(Req, State=#state{get_rules=Rules}) ->
	case query_data(Rules, Req) of
		{ok, Data, Req2} ->
			{true, Req2, State#state{data=Data}};
		{error, notfound} ->
			{false, Req, State}
	end.

content_types_accepted(Req, State) ->
	{[
		{<<"application/x-www-form-urlencoded;charset=UTF-8">>,
			from_form},
		{<<"application/json">>, from_json}
	], Req, State}.



to_html(Req, State=#state{template=Module, data=Data}) ->
	{ok,Body} = Module:render(Data),
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
from_form(Req, State=#state{put_rules=Rules}) ->
	process_rules(Rules,Req),
	{true, Req, State}.
		


process_rules(Rules,Req) ->
	process_rules(Rules,Req,[]).

process_rules([],_Req,_Acc) ->
	[];

process_rules([Rule|Tail],Req,Acc) ->
	case rule(Rule,Req) of
		E = {error, _} ->
			E;
		{Field, Req2} ->
			process_rules(Tail, Req2, [Field|Acc])
	end.

rule({StatusFld,{put,Bucket,{binding,Binding}}},Req) ->
	Bucket2 = atom_to_binary(Bucket, latin1),
	{Key, Req2} = cowboy_req:binding(Binding, Req),
	{ok, Values, Req4} = cowboy_req:body_qs(Req2),
	case fw_userdata_server:set_data(Bucket2,Key,<<"test">>,jsx:encode(Values),<<"test">>) of
		ok ->
			{{StatusFld, <<"Successfully inserted/updated">>}, Req4};
		E = {error, _} ->
			E
	end;

rule({StatusFld,{put,Bucket,{binding,Binding},{qs_property,Prop}}},Req) ->
	Bucket2 = atom_to_binary(Bucket, latin1),
	{Key, Req2} = cowboy_req:binding(Binding, Req),
	{ok, Values, Req4} = cowboy_req:body_qs(Req2),
	{Prop, Value} = lists:keyfind(Prop, 1, Values),
	case fw_userdata_server:set_data(Bucket2,Key,<<"test">>,jsx:encode(Value),<<"test">>) of
		ok ->
			{{StatusFld, <<"Successfully inserted/updated">>}, Req4};
		E = {error, _} ->
			E
	end;
rule({StatusFld,{put,Bucket,{qs_binding,Binding},{qs_property,Prop}}},Req) ->
	Bucket2 = atom_to_binary(Bucket, latin1),
	{ok, Values, Req4} = cowboy_req:body_qs(Req),
	{Binding, Key} = lists:keyfind(Binding, 1, Values),
	{Prop, Value} = lists:keyfind(Prop, 1, Values),
	case fw_userdata_server:set_data(Bucket2,Key,<<"test">>,jsx:encode(Value),<<"test">>) of
		ok ->
			{{StatusFld, <<"Successfully inserted/updated">>}, Req4};
		E = {error, _} ->
			E
	end;

rule({redirect,URL},Req) ->
	{{redirect,URL},Req}.
	%% @todo
%% @todo template_from_json

delete_resource(Req, State) ->
	%% @todo
	io:format("delete_resource ~p~n", [Req]),
	{true, Req, State}.

delete_completed(Req, State) ->
	%% @todo
	{true, Req, State}.


query_data(Rules, Req) ->
	query_data(Rules, Req, []).
query_data([], Req, Acc) ->
	{ok, Acc, Req};
query_data([Rule|Tail], Req, Acc) ->
	case query_field(Rule, Req) of
		E = {error, _} ->
			E;
		{Field, Req2} ->
			query_data(Tail, Req2, [Field|Acc])
	end.

query_field({Name, {get, Bucket, {binding, Binding}}}, Req) ->
	Bucket2 = atom_to_binary(Bucket, latin1),
	{Key, Req2} = cowboy_req:binding(Binding, Req),
	case fw_userdata_server:get_value(Bucket2, Key) of
		{ok, JSON} ->
			{{Name, jsx:decode(JSON)}, Req2};
		E = {error, _} ->
			E
	end;
query_field({Name, {get, Bucket, path_info}}, Req) ->
	Bucket2 = atom_to_binary(Bucket, latin1),
	{Key, Req2} = cowboy_req:path_info(Req),
	case fw_userdata_server:get_value(Bucket2, Key) of
		{ok, JSON} ->
			{{Name, jsx:decode(JSON)}, Req2};
		E = {error, _} ->
			E
	end;
query_field({Name, {get_all, Bucket}}, Req) ->
	Bucket2 = atom_to_binary(Bucket, latin1),
	{ok, ValuesList} = fw_userdata_server:get_all_values(Bucket2),
	{{Name, ValuesList}, Req};
query_field({Name, {mfa, {M, F, A}}}, Req) ->
	{ok, ValuesList} = apply(M, F, A),
	{{Name, ValuesList}, Req}.
