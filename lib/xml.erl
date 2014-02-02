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

%% NOTE: Because I can never remember how to use xmerl and do not need
%% a full DOM representation in 90% of practical circumstances for this 
%% document markup language which has found its way, incorrectly, into 
%% data processing.

-module(xml).

%-include("ice.hrl").
-define(is_string(X), is_list(X) andalso is_integer(hd(X))).

-export([decode/1, encode/1, xpath/2, format/1]).
-export([get_attribute/2, get_attribute/3]).

-define(XML_REGEX, <<"[\t\n\r]|(<[^>]*>)">>).
-define(ELEMENT_REGEX, <<"([0-9A-Za-z:-_]+=\"[^\"]*\")">>).
-define(SP, 32).
%% unused
% -record(element, {tag, attrs, body}).

%% TODO: Improve scope, add validation?
%% OR: forget the w3c specs and just make something useful?
%% NOTE: This implementation strips out comments

%%
encode(Xml) ->
	encode_document(Xml).

%%
decode(Bin) ->
	decode_document(Bin).
	
%%
xpath(Path, Xml) ->
	xml_xpath:value(Path, Xml).
%%
format(Xml) ->
	xml_format:format(Xml).
	
%%
get_attribute(Name, Element) ->
	get_attribute(Name, Element, undefined).
%%
get_attribute(Name, {_, Attrs, _}, Default) ->
	opts:get(Name, Attrs, Default).
	
%% Internal functions

%%
encode_document({xml, Attrs, Content}) ->
	ABin = list_to_binary(encode_attrs(Attrs, [])),
	Bin = encode_document(Content),
	list_to_binary([<<"<?xml", ABin/binary, "?>">>, Bin]);
encode_document([Term]) -> 
	encode_element(Term).

encode_element({Name, Content}) ->
	encode_element({Name, [], Content});
encode_element({{NS, Name}, Attrs, Content}) ->
	Prefix = text:format(NS),
	case is_binary(Name) of
	true -> 
		Suffix = Name;
	false -> 
		Suffix = text:format(Name)
	end,
	encode_element({list_to_binary([Prefix, $:, Suffix]), Attrs, Content});
encode_element({Name, Attrs, Content}) ->
	%?TTY({encode, Name}),
	StartTag = [$<, text:encode(Name), encode_attrs(Attrs, [])],
	Element = 
		case encode_content(Content, []) of
		[] -> 
			[StartTag, $/, $>];
		Value -> 
			[StartTag, $>, Value, $<, $/, text:encode(Name), $>]
		end,
	list_to_binary(Element);
encode_element(Content) ->
	text:encode(Content).
%%
encode_attrs([{version, V}|T], Acc) when is_tuple(V) ->
	Attr = list_to_binary([<<" version">>, <<$=, $">>, version:encode(V), <<$">>]),
	encode_attrs(T, [Attr|Acc]);
encode_attrs([{K, V}|T], Acc) ->
	case K of
	{NS, Name} ->
		Prefix = text:format(NS),
		Suffix = 
			case is_binary(Name) of
			true -> Name;
			false -> text:format(Name)
		end,
		K0 = list_to_binary([Prefix, $:, Suffix]);
	_ ->
		K0 = text:encode(K)
	end,
	Attr = list_to_binary([<<$ >>, K0, <<$=, $">>, text:encode(V), <<$">>]),
	encode_attrs(T, [Attr|Acc]);
encode_attrs([], Acc) ->
	lists:reverse(Acc).
%%
encode_content([H|T], Acc) ->
	encode_content(T, [encode_element(H)|Acc]);
encode_content([], Acc) ->
	lists:reverse(Acc).

%%
decode_document(File) when ?is_string(File) ->
	{ok, Bin} = file:read_file(File),
	decode_document(Bin);
decode_document(Bin) when is_binary(Bin) ->
	List = text:split(Bin, ?XML_REGEX),
	{Root, []} = decode_elements(List, [], []),
	case Root of
	[X = {xml, _Attrs, []}|E] ->
		Version = version:decode(get_attribute(version, X, <<"1.0">>)),
		{xml, [{version, Version}], E};
	E = [_] ->
		{xml, [{version, {1, 0}}], E};
	_ ->
		{error, too_many_roots}
	end.

%%
decode_elements([Terminal|T], Terminal, Acc) ->
	{lists:reverse(Acc), T};
decode_elements([<<"<!--", Bin/binary>>|T], Terminal, Acc) ->
	T0 = decomment([Bin|T],[]),
	decode_elements(T0, Terminal, Acc);
decode_elements([<<"<![CDATA[", Bin/binary>>|T], Terminal, Acc) ->
	{CData, T0} = cdata([Bin|T],[]),
	decode_elements(T0, Terminal, [{cdata, [], CData}|Acc]);
decode_elements([<<$<, $?, Bin/binary>>|T], Terminal, Acc) ->
	[Tag, <<>>] = text:split(Bin, <<"\\?>">>, 2),
	R = text:split(Tag, ?ELEMENT_REGEX),
	[Name|Attrs] = text:trim(R),
	Pairs = decode_attrs(Attrs, []),
	decode_elements(T, Terminal, [{xml_namespace:name(Name), Pairs, []}|Acc]);
decode_elements([<<$<, Bin/binary>>|T], Terminal, Acc) ->
	[Tag, Close] = text:split(Bin, <<"(>|/>)">>, 2),
	R = text:split(Tag, ?ELEMENT_REGEX),
	[Name|Attrs] = text:trim(R),
	Pairs = decode_attrs(Attrs, []),
	case Close of
	<<"/>">> -> 
		decode_elements(T, Terminal, [{xml_namespace:name(Name), Pairs, []}|Acc]);
	<<">">> ->
		ChildTerminal = <<"</", Name/binary, ">">>,
%		io:format("T: ~p~n", [Terminal]),
%		{BodyList, [Terminal|T1]} = lists:splitwith(fun(X) -> X =/= Terminal end, T),
		{Body, Rest} = decode_elements(T, ChildTerminal, []),
		decode_elements(Rest, Terminal, [{xml_namespace:name(Name), Pairs, Body}|Acc])
	end;
decode_elements([H|T], Terminal, Acc) ->
	case text:trim(H) of %% remove whitespace -- make optional?
	<<>> ->
		decode_elements(T, Terminal, Acc);
	Value ->
%		decode_elements(T, Terminal, [make_key(Value)|Acc])
		decode_elements(T, Terminal, [Value|Acc])
	end;
decode_elements([], _Terminal, Acc) ->
	{lists:reverse(Acc), []}.
%%
decode_attrs([H|T], Acc) ->
	[Name, Value] = text:split(H, <<$=>>, 2),
	decode_attrs(T, [{xml_namespace:name(Name), text:unquote(Value)}|Acc]);
decode_attrs([], Acc) -> 
	lists:reverse(Acc).

decomment([H|T], Acc) ->
	case re:run(H, <<"-->$">>) of
	{match, _} ->
		T;
	nomatch ->
		decomment(T, Acc)
	end.
	
cdata([H|T], Acc) ->
	case re:run(H, <<"]]>$">>) of
	{match, _} ->
		Length = byte_size(H) - 3,
		<<Data:Length/binary, _:3/binary>> = H,
		{lists:reverse([Data|Acc]), T};
	nomatch ->
		cdata(T, [H|Acc])
	end.
