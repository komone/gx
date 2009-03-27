%%
%% GX Framework
%% Copyright 2009 <steven.charles.davis@gmail.com>. All rights reserved.
%% LICENSE: The correct license type has not yet been determined.
%%
-module(gx_xml).
-author('steve@simulacity.com').
-vsn("0.2").

-include_lib("xmerl/include/xmerl.hrl").
-export([load/1, generate/1, generate/2]).
%
% TODO: When the DTD/Schema is stable, add validation.
%

%% Utility function to generate gx '.gui' term file from GXML
%% NOTE: Called by gx:export/1, gx:export/2
%% IMPL: Using ~w instead of ~p would reduce the .gui file size
generate(GxmlFile) ->
	TermFile = filename:basename(GxmlFile, ".xml") ++ ".gui",
	generate(GxmlFile, TermFile).
%
generate(GxmlFile, TermFile) ->
	{ok, UI} = load(GxmlFile),
	Terms = [io_lib:format("~p.~n", [Component]) || Component <- UI],
	ok = file:write_file(TermFile, Terms).

%% Loads a GXML file and returns it as a valid GX Term
%% NOTE: this returns a list as you may wish to use more than one window, 
%% specify dialogs, or have replaceable component trees at runtime...
load(GxmlFile) ->
	{ok, Bin} = file:read_file(GxmlFile),
	Markup = binary_to_list(Bin),
	{Xml, []} = xmerl_scan:string(Markup, [{space, normalize}]),
	case Xml#xmlElement.name of 
	gxml -> 
		Terms = convert(Xml#xmlElement.content, []),
		GxTerms = collapse(Terms, []),
		{ok, GxTerms};
	_ -> {error, invalid_file}
	end.

%%
%% Internal API
%%

%% Transform the scanned XMERL term  to a GX term
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

%% All attributes present as strings from xmerl, here we convert booleans and
%% integers to thier fundamental type
%% IMPL: what about boolean names that were intended to be strings?
%% TODO: perhaps later, atoms etc too?
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
	
%% 'collapse' postprocesses text and item elements in the term generated 
%% by 'convert', resulting in a valid GX term
%% 1) Collapses 'item" tuples into a list of strings as the 'items' option
%% 2) Moves the first text value of the child into the parent 'value' option
%% TODO: This series of patterns is somewhat fragile as it introduces an 
%% explicit dependency on component type names, affecting maintenance
%% IMPL: GXML may contain more than one xmlText element, however we only take
%% the *first* xmlText element as the 'value' attribute
collapse([Component = {list, _, _} | T], Acc) ->
	collapse(T, [collapse_items(Component) | Acc]);
collapse([Component = {choice, _, _} | T], Acc) ->
	collapse(T, [collapse_items(Component) | Acc]);
collapse([Component = {combo, _, _} | T], Acc) ->
	collapse(T, [collapse_items(Component) | Acc]);
collapse([Component = {radiobox, _, _} | T], Acc) ->
	collapse(T, [collapse_items(Component) | Acc]);
collapse([Component = {checklist, _, _} | T], Acc) ->
	collapse(T, [collapse_items(Component) | Acc]);
collapse([{Type, Opts, Children}|T], Acc) ->
	Kids = [X || X <- Children, is_tuple(X)],
	Strings = [X || X <- Children, is_list(X)],
	Opts1 = collapse_text(Strings, Opts),
	Kids1 = collapse(Kids, []),
	collapse(T, [{Type, Opts1, Kids1}|Acc]);
collapse([], Acc) ->
	lists:reverse(Acc).

%% Items are top-level elements in GXML, but as GX terms they should be a list
%% of string-labeled 'items'
collapse_items({Type, Opts, Children}) ->
	Items = [collapse_item(Attrs, Value) || {item, Attrs, Value} <- Children],
	{Type, [{items, Items}|Opts], []}.
%	
collapse_item(Attrs, []) -> proplists:get_value(label, Attrs);
collapse_item(_, [Text|_]) when is_list(Text) -> Text.

%% IMPL: Takes only the first text element
collapse_text([Text|_], Opts) -> [{value, Text}|Opts];
collapse_text([], Opts) -> Opts.
