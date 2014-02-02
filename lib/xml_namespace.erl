%% Copyright 2011-2014 Steve Davis <steve@simulacity.com>
%
% Licensed under the Apache License, Version 2.0 (the "License");
% you may not use this file except in compliance with the License.
% You may obtain a copy of the License at
% 
% http://www.apache.org/licenses/LICENSE-2.0
% 
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS,
% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
% See the License for the specific language governing permissions and
% limitations under the License.

-module(xml_namespace).

-export([name/1, name/2, encode_name/1, decode_name/1]).

%% TEMP
-compile(export_all).

-define(NS_MAP, [
	{<<"http://www.w3.org/2000/10/XMLSchema">>, <<"xsd">>}, 
	{<<"http://www.w3.org/2001/XMLSchema">>, <<"xs">>}, 
	{<<"http://schemas.xmlsoap.org/wsdl/">>, <<"wsdl">>},
	{<<"http://schemas.xmlsoap.org/wsdl/soap/">>, <<"soap">>},
	{<<"http://schemas.xmlsoap.org/soap/http">>, <<"http">>},
	{<<"http://schemas.xmlsoap.org/soap/envelope/">>, <<"soapenv">>}
]).

test() ->
	test("work/wsdl/StockQuote.wsdl").

test(File) ->
	Bin = ifile:load(File),
	Xml = xml:decode(Bin),
	normalize(Xml), 
	ok.
	
normalize(Xml) ->
	normalize(Xml, undefined, dict:new()).
normalize({xml, _, Xml}, DefaultNS, Namespaces) ->
	normalize(Xml, DefaultNS, Namespaces, []).
	
normalize([{Name, Attrs, Content}|T], Prefix, Namespaces, Acc) ->
	Namespaces0 = update_namespaces(Attrs, Namespaces),
	{Prefix0, Attrs0} = get_prefix(Prefix, Namespaces, Attrs),
	QName = qualify_name(Name, Prefix0),
	Content0 = normalize(Content, Prefix0, Namespaces0, []),
	normalize(T, Prefix, Namespaces, [{QName, Attrs0, Content0}|Acc]);
normalize([H|T], Prefix, Namespaces, Acc) when is_binary(H) ->
	normalize(T, Prefix, Namespaces, Acc);	
normalize([], _, _, Acc) ->
	lists:reverse(Acc).

update_namespaces([{{xmlns, Prefix}, URN}|T], NS) ->
	NS0 = dict:store(URN, Prefix, NS),
	%?TTY({updated, Prefix, dict:to_list(NS0)}),
	update_namespaces(T, NS0);
update_namespaces([_|T], NS) ->
	update_namespaces(T, NS);
update_namespaces([], NS) ->
	NS.

qualify_name(Name, undefined) ->
	Name;
qualify_name({Prefix, Name}, _) ->
	{Prefix, Name};
qualify_name(Name, Prefix) ->
	{Prefix, Name}.

get_prefix(Prefix, NS, Attrs) ->
	case opts:get(xmlns, Attrs) of
	undefined ->
		{Prefix, Attrs};
	Value ->
		case dict:find(Value, NS) of
		{ok, Prefix0} ->
			{Prefix0, Attrs};
		_ ->
			case lists:keyfind(Value, 1, ?NS_MAP) of
			{Value, Prefix0} ->
				{Prefix0, [{{xmlns,Prefix0}, Value}|Attrs]};
			false ->
				{undefined, Attrs}
			end
		end
	end.
	
%% xmlns
name(Name) ->
	case text:split(Name, <<$:>>) of
	[Prefix, Key] ->
		{make_atom(Prefix), make_atom(Key)};
	[Key] ->
		make_atom(Key)
	end.
		
%%
make_atom(Name) ->
	try 
		binary_to_existing_atom(Name, utf8) 
	catch
	_:_ -> 
		Name
	end.
		
name(undefined, Name) ->
	text:format(Name);
name(NS, Name) ->
	Prefix = text:format(NS),
	Suffix = text:format(Name),
	<<Prefix/binary, $:, Suffix/binary>>.

%% encode_name(atom() | binary()) -> binary().
encode_name(X) when is_atom(X) ->
	encode_name(atom_to_binary(X, utf8));
encode_name(<<X, Rest/binary>>) ->
	encode_name(Rest, [text:to_upper(X)]).
%	
encode_name(<<$_, X, Rest/binary>>, Acc) ->
	encode_name(Rest, [text:to_upper(X) | Acc]);
encode_name(<<X, Rest/binary>>, Acc) ->
	encode_name(Rest, [X | Acc]);
encode_name(<<>>, Acc) ->
	list_to_binary(lists:reverse(Acc)).

%% decode_name(binary()) -> atom().
decode_name(Name) when is_binary(Name) ->
	case text:split(Name, <<":">>) of
	[Prefix, Text] ->
		NS = decode_name(binary_to_list(Prefix), []),
		Name0 = decode_name(binary_to_list(Text), []),
		{NS, Name0};
	[Text] ->
		decode_name(binary_to_list(Text), [])
	end;
decode_name(_) ->
	undefined.
%
decode_name([H|T], []) ->
	decode_name(T, [text:to_lower(H)]);
decode_name([H|T], Acc) ->
	case text:is_upper(H) of
	true ->
		decode_name(T, [text:to_lower(H), $_ | Acc]);
	false ->
		decode_name(T, [H|Acc])
	end;
decode_name([], Acc) ->
	Name = list_to_binary(lists:reverse(Acc)),
	binary_to_atom(Name, utf8).	

