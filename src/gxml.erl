%%
%%
-module(gxml).
-author('steve@simulacity.com').

-include_lib("xmerl/include/xmerl.hrl").
-export([load/1]).

%% Loads a gxml file and returns it as a gx term
% NOTE: this returns a list as you may wish to use 
% more than one window or have replaceable trees for
% use later...
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
	
%%
%% Internal API
%%
convert([H = #xmlElement{} | T], Acc) ->
	Name = H#xmlElement.name,
	Attributes = convert(H#xmlElement.attributes, []),
	Children = convert(H#xmlElement.content, []),
	convert(T, [{Name, Attributes, Children} | Acc]);
convert([H = #xmlAttribute{} | T], Acc) ->
	Value = get_value(H#xmlAttribute.name, H#xmlAttribute.value),
	convert(T, [Value | Acc]);
convert([#xmlText{value=" "} | T], Acc) ->
	convert(T, Acc);
convert([H = #xmlText{} | T], Acc) ->
	Text = H#xmlText.value,
	convert(T, [Text | Acc]);
convert([_|T], Acc) ->
	convert(T, Acc);
convert([], Acc) ->
	lists:reverse(Acc).

%% convert booleans, integers (and later do atoms too)
get_value(Name, "true") -> {Name, true};
get_value(Name, "false") -> {Name, false};
get_value(Name, Value) when is_list(Value) -> 
	try
		{Name, list_to_integer(Value)}
	catch _:_ -> 
		{Name, lists:flatten(Value)}
	end;
get_value(Name, Value) ->
	{Name, Value}.
