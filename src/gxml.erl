%%
%%
-module(gxml).
-author('steve@simulacity.com').

-include_lib("xmerl/include/xmerl.hrl").
-export([load/1]).

load(File) ->
	{ok, Bin} = file:read_file(File),
	Markup = binary_to_list(Bin),
	{Xml, []} = xmerl_scan:string(Markup, [{space, normalize}]),
	case Xml#xmlElement.name of 
	gxml -> 
		Term = convert(Xml#xmlElement.content, []),
		{ok, Term};
	_ -> {error, invalid_file}
	end.

convert([H = #xmlElement{} | T], Acc) ->
	Name = H#xmlElement.name,
	Attributes = convert(H#xmlElement.attributes, []),
	Children = convert(H#xmlElement.content, []),
	convert(T, [{Name, Attributes, Children} | Acc]);
convert([H = #xmlAttribute{} | T], Acc) ->
	Name = H#xmlAttribute.name,
	Value = H#xmlAttribute.value,
	case Name of 
	width  -> Value1 = list_to_integer(Value);
	height -> Value1 = list_to_integer(Value);
	command -> Value1 = list_to_integer(Value);
	enable -> Value1 = (Value == "true");
	_ -> Value1 = lists:flatten(Value)
	end,
	convert(T, [{Name, Value1} | Acc]);
convert([#xmlText{value=" "} | T], Acc) ->
	convert(T, Acc);
convert([H = #xmlText{} | T], Acc) ->
	Text = H#xmlText.value,
	convert(T, [Text | Acc]);
convert([_|T], Acc) ->
	convert(T, Acc);
convert([], Acc) ->
	lists:reverse(Acc).
