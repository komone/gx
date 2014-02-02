%% Copyright 2010-2014 Steve Davis <steve@simulacity.com>
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

-module(gx_xml).

-include("gx.hrl").

-export([load/1, generate/1]).
-export([decode/1]).
-define(GUI_FILE_EXT, <<".gui">>).
%
% TODO: When the DTD/Schema is stable, add validation.
% NOTE: Implementation will currently leak atoms

%% Utility function to generate gx '.gui' term file from GXML
%% NOTE: Called by gx:export/1, gx:export/2
%% IMPL: Using ~w instead of ~p would reduce the .gui file size
generate(GxmlFile) ->
	Bin = path:load(GxmlFile),
	Xml = xml:decode(Bin),
	Term = decode(Xml),
	Filename = <<(path:basename(GxmlFile))/binary, ?GUI_FILE_EXT/binary>>,
	Path = path:new(Filename),
	ok = path:save(Path, term_to_binary(Term)).

%% Loads a GXML file and returns it as a valid GX Term
%% NOTE: this returns a list as you may wish to use more than one window, 
%% specify dialogs, or have replaceable component trees at runtime...
load(GxmlFile) ->
	Bin = path:load(GxmlFile),
	{xml, _, [Doc]} = xml:decode(Bin),
	decode(Doc).
	
%%
decode({gxml, A, E}) ->
	ID = get_atom(id, A, gx),
	Mod = get_atom(module, A, gx_runner),
	Components = decode(E, []),
	Path = get_option(path, A),
	#gui{id = ID, module = Mod, def = Components, path = Path};
decode({'glade-interface', _, _}) -> 
	{not_supported, gtk_glade};
decode({application, _, _}) ->
	{not_supported, wx_glade};
decode(_) -> 
	{error, invalid_file_type}.
	
% TEMP alias
decode([{window, A, C}|T], Acc) ->
	decode([{frame, A, C}|T], Acc);
decode([{frame, A, C}|T], Acc) ->
	ID = get_atom(id, A),
	Title = get_option(title, A, <<"Untitled">>),
	Icon = get_option(icon, A),
	Pos = get_pos(A),
	Width = get_integer(width, A, -1),
	Height = get_integer(height, A, -1),
	Kids = decode(C, []),
	Callbacks = get_callbacks(A),
	Record = #frame{id = ID, pos = Pos, title = Title, icon = Icon, 
		size = {Width, Height}, callbacks = Callbacks, content = Kids},
	decode(T, [Record|Acc]);
decode([{dialog, A, C}|T], Acc) ->
	ID = get_atom(id, A),
	Parent = get_atom(parent, A),
	Title = get_option(title, A, <<"Untitled">>),
	Icon = get_option(icon, A),
	Pos = get_pos(A),
	Width = get_integer(width, A, -1),
	Height = get_integer(height, A, -1),
	Kids = decode(C, []),
	Callbacks = get_callbacks(A),
	Show = get_boolean(show, A, false),
	Record = #dialog{id = ID, ref = Parent, pos = Pos, title = Title, icon = Icon, 
		size = {Width, Height}, show = Show, callbacks = Callbacks, content = Kids},
	decode(T, [Record|Acc]);
	
decode([{menubar, A, C}|T], Acc) ->
	ID = get_atom(id, A),
	Kids = decode(C, []),
	Record = #menubar{id = ID, content = Kids},
	decode(T, [Record|Acc]);
decode([{menu, A, C}|T], Acc) ->
	ID = get_atom(id, A),
	Label = get_option(label, A),
	Icon = get_option(icon, A),
	Items = get_items(menu, C),
	Kids = decode(Items, []),
	Record = #menu{id = ID, label = Label, icon = Icon, content = Kids},
	decode(T, [Record|Acc]);
decode([{menuitem, A, []}|T], Acc) ->
	ID = get_atom(id, A),
	Type = get_atom(type, A, normal),
	Label = get_option(label, A),
	Icon = get_option(icon, A),
	Enabled = get_boolean(enabled, A, true),
	Checked = get_boolean(checked, A, false),
	Callbacks = get_callbacks(A),
	Record = #menuitem{id = ID, type = Type, label = Label, icon = Icon, 
		enabled = Enabled, checked = Checked, callbacks = Callbacks},
	decode(T, [Record|Acc]);
decode([{separator, _A, []}|T], Acc) ->
	Record = #separator{},
	decode(T, [Record|Acc]);
decode([{toolbar, A, C}|T], Acc) ->
	ID = get_atom(id, A),
	Items = get_items(toolbar, C),
	Kids = decode(Items, []),	
	Record = #toolbar{id = ID, content = Kids},
	decode(T, [Record|Acc]);
decode([{toolbutton, A, []}|T], Acc) ->
	ID = get_atom(id, A),
	Icon = get_option(icon, A),
	Label = get_option(label, A, <<>>),
	Callbacks = get_callbacks(A),
	Record = #toolbutton{id = ID, label = Label, icon= Icon, callbacks = Callbacks},
	decode(T, [Record|Acc]);
decode([{statusbar, A, C}|T], Acc) ->
	ID = get_atom(id, A),
	Message = get_text(message, A, C),
	Record = #statusbar{id = ID, message = Message},
	decode(T, [Record|Acc]);
	
decode([{panel, A, C}|T], Acc) ->
	ID = get_atom(id, A),
	Width = get_integer(width, A, -1),
	Height = get_integer(height, A, -1),
	Layout = get_atom(layout, A, column),
	Color = get_atom(color, A), %% color names only
	Background = get_option(background, A, undefined),
	Align = get_atom(align, A, left),
	Fill = get_atom(fill, A, both),
	Border = get_integer(border, A, 0),
	Rows = get_integer(rows, A, 0),
	Cols = get_integer(cols, A, 0),
	Kids = decode(C, []),
	Record = #panel{id = ID, size = {Width, Height}, layout = Layout, rows = Rows, cols = Cols,
		color = Color, background = Background, align = Align, border = Border, fill = Fill, content = Kids},
	decode(T, [Record|Acc]);
decode([{box, A, C}|T], Acc) ->
	ID = get_atom(id, A),
	Width = get_integer(width, A, -1),
	Height = get_integer(height, A, -1),
	Label = get_option(label, A),
	Layout = get_atom(layout, A, column),
	Align = get_atom(align, A, left),
	Fill = get_atom(fill, A, both),
	Border = get_integer(border, A, 0),
	Kids = decode(C, []),
	Record = #box{id = ID, label = Label, size = {Width, Height}, align = Align, 
		layout = Layout, border = Border, fill = Fill, content = Kids},
	decode(T, [Record|Acc]);
decode([{splitpane, A, C}|T], Acc) ->
	ID = get_atom(id, A),
	Width = get_integer(width, A, -1),
	Height = get_integer(height, A, -1),
	Label = get_option(label, A),
	Layout = get_atom(layout, A, vertical),
	Align = get_atom(align, A, left),
	Fill = get_atom(fill, A, both),
	Border = get_integer(border, A, 0),
	Sash = get_integer(sash, A, 0),
	% also: sash width?
	Gravity = get_integer(gravity, A, 50),
	Kids = decode(C, []),
	Record = #splitpane{id = ID, label = Label, size = {Width, Height}, align = Align, 
		layout = Layout, border = Border, fill = Fill, sash = Sash, gravity = Gravity, content = Kids},
	decode(T, [Record|Acc]);
decode([{grid, A, []}|T], Acc) ->
	ID = get_atom(id, A),
	Rows = get_integer(rows, A, 1),
	Cols = get_integer(cols, A, 1),
	Align = get_atom(align, A, left),
	Fill = get_atom(fill, A, both),
	Border = get_integer(border, A, 0),
	Record = #grid{id = ID, rows = Rows, cols = Cols, align = Align, fill = Fill, border = Border},
	decode(T, [Record|Acc]);	
decode([{line, A, []}|T], Acc) ->
	Orientation = get_atom(orientation, A, horizontal),
	Record = get_control_record(line, A, [Orientation]),
	decode(T, [Record|Acc]);
decode([{text, A, C}|T], Acc) ->
	Justify = get_atom(justify, A, left),
	Wrap = get_integer(wrap, A, -1),
	Fixed = get_boolean(fixed, A, false),
	Font = get_option(font, A),
	Text = list_to_binary([<<X/binary, " ">> || X <- C, is_binary(X)]),
	Record = get_control_record(text, [{label, Text}|A], [Justify, Wrap, Fixed, Font]),
	decode(T, [Record|Acc]);
decode([{image, A, []}|T], Acc) ->
	Source = get_option(path, A),
	Record = get_control_record(image, A, [Source]),
	decode(T, [Record|Acc]);
decode([{progressbar, A, []}|T], Acc) ->
	Layout = get_atom(layout, A, horizontal),
	Smooth = get_boolean(smooth, A, false),
	Percent = get_integer(percent, A, -1),
	Record = get_control_record(progressbar, A, [Layout, Smooth, Percent]),
	decode(T, [Record|Acc]);
decode([{date, A, []}|T], Acc) ->
	Min = get_date(min, A, undefined),
	Max = get_date(max, A, undefined),
	Value = get_date(value, A, {date(), time()}),
	Record = get_control_record(date, A, [Min, Max, Value]),
	decode(T, [Record|Acc]);
	
decode([{slider, A, []}|T], Acc) ->
	Min = get_integer(min, A, 0),
	Max = get_integer(max, A, 10),
	Value = get_integer(value, A, 0),
	Ticks = get_boolean(ticks, A, false),
	Labels = get_boolean(labels, A, false),
	Record = get_control_record(slider, A, [Min, Max, Value, Ticks, Labels]),
	decode(T, [Record|Acc]);	
decode([{spinner, A, []}|T], Acc) ->
	Min = get_integer(min, A, -1),
	Max = get_integer(max, A, -1),
	Value = get_integer(value, A, -1),
	Wrap = get_boolean(wrap, A, false),
	Record = get_control_record(spinner, A, [Min, Max, Value, Wrap]),
	decode(T, [Record|Acc]);	
decode([{button, A, [C]}|T], Acc) ->
	decode([{button, [{label, C}|A], []}|T], Acc);
decode([{button, A, []}|T], Acc) ->
	Icon = get_option(icon, A),
	Record = get_control_record(button, A, [Icon]),
	decode(T, [Record|Acc]);
decode([{togglebutton, A, []}|T], Acc) ->
	Icon = get_option(icon, A),
	Record = get_control_record(togglebutton, A, [Icon]),
	decode(T, [Record|Acc]);
decode([{checkbox, A, [C]}|T], Acc) ->
	decode([{checkbox, [{label, C}|A], []}|T], Acc);
decode([{checkbox, A, []}|T], Acc) ->
	Justify = get_atom(justify, A, left),
	Value = get_boolean(value, A, false),
	Record = get_control_record(checkbox, A, [Justify, Value]),
	decode(T, [Record|Acc]);
decode([{picker, A, []}|T], Acc) ->
	Type = get_atom(type, A, undefined),
	Value = get_option(value, A, <<"/">>),
	Record = get_control_record(picker, A, [Type, Value]),
	decode(T, [Record|Acc]);
decode([{checklist, A, C}|T], Acc) ->
	Selected = get_selected_item(C),
	Items = get_items(checklist, C),
	Record = get_control_record(checklist, A, [Items, Selected]),
	decode(T, [Record|Acc]);
decode([{radiobox, A, C}|T], Acc) ->
	Selected = get_selected_item(C),
	Items = get_items(radiobox, C),
	Record = get_control_record(radiobox, A, [Items, Selected]),
	decode(T, [Record|Acc]);
decode([{choice, A, C}|T], Acc) ->
	Selected = get_selected_item(C),
	Items = get_items(choice, C),
	Record = get_control_record(choice, A, [Items, Selected]),
	decode(T, [Record|Acc]);
decode([{combo, A, C}|T], Acc) ->
	Selected = get_selected_item(C),
	Items = get_items(combo, C),
	Record = get_control_record(combo, A, [Items, Selected]),
	decode(T, [Record|Acc]);
decode([{list, A, C}|T], Acc) ->
	Selected = get_selected_item(C),
	Items = get_items(list, C),
	Record = get_control_record(list, A, [Items, Selected]),
	decode(T, [Record|Acc]);
decode([{tabs, A, C}|T], Acc) ->
	Icon = get_option(icon, A),
	Kids = decode(C, []),
	Record = get_control_record(tabs, A, [Icon, Kids]),
	decode(T, [Record|Acc]);
decode([{tab, A, C}|T], Acc) ->
	Icon = get_option(icon, A),
	Kids = decode(C, []),
	Record = get_control_record(tab, A, [Icon, Kids]),
	decode(T, [Record|Acc]);	
decode([{tree, A, C}|T], Acc) ->
	Icon = get_option(icon, A),
	Items = get_items(tree, C),
	Kids = decode(Items, []),
	Data = undefined, %% TEMP
	Record = get_control_record(tree, A, [Icon, Kids, Data]),
	decode(T, [Record|Acc]);	
decode([{treeitem, A, C}|T], Acc) ->
	Id = get_atom(id, A),
	Icon = get_option(icon, A),
	case get_option(label, A) of
	undefined ->	
		[Label] = C,
		Kids = [];
	Value ->
		Label = Value,
		Items = get_items(tree, C),
		Kids = decode(Items, [])
	end,
	decode(T, [#treeitem{id = Id, label = Label, icon = Icon, content = Kids}|Acc]);	
decode([{treebook, A, C}|T], Acc) ->
	Kids = decode(C, []),
	Record = get_control_record(treebook, A, [Kids]),
	decode(T, [Record|Acc]);
decode([{calendar, A, _}|T], Acc) ->
	Record = get_control_record(calendar, A, []),
	decode(T, [Record|Acc]);	
decode([{filetree, A, _}|T], Acc) ->
	Path = get_option(path, A),
	Record = get_control_record(filetree, A, [Path]),
	decode(T, [Record|Acc]);	
decode([{input, A, C}|T], Acc) ->
	Style = get_atom(style, A, normal),
	Enabled = get_boolean(enabled, A, true),
	Source = get_option(src, A),
	case C of
	[Value|_] when is_binary(Value) ->
		Content = Value;
	_ -> 
		Content = <<>>
	end,
	Record = get_control_record(input, A, [Style, Enabled, Source, Content]),
	decode(T, [Record|Acc]);	
decode([{editor, A, _C}|T], Acc) ->
	Lexer = get_atom(lexer, A, none),
	Record = get_control_record(editor, A, [Lexer]),
	decode(T, [Record|Acc]);	
decode([], Acc) ->
	lists:reverse(Acc).

%%
get_option(Key, Attrs) ->
	get_option(Key, Attrs, undefined).	
get_option(Key, [{Key, Value}|_], _) ->
	Value;
get_option(Key, [_|T], Default) ->
	get_option(Key, T, Default);
get_option(_, [], Default) ->
	Default.

%%
get_atom(Key, Attrs) ->
	get_atom(Key, Attrs, undefined).	
get_atom(Key, Opts, Default) -> 
	case get_option(Key, Opts, Default) of 
	Value when is_atom(Value) -> 
		Value;
	Value when is_binary(Value) -> 
		binary_to_atom(Value, utf8)
	end.
	
get_boolean(Key, Opts, Default) ->
	case get_atom(Key, Opts, undefined) of
	true -> 
		true;
	false -> 
		false;
	_ -> 
		Default
	end.
	
get_integer(Key, Opts, Default) ->
	case get_option(Key, Opts, Default) of 
	Value when is_integer(Value) -> 
		Value;
	Value when is_binary(Value) -> 
		list_to_integer(binary_to_list(Value))
	end.
	
get_pos(Opts) ->
	case get_atom(pos, Opts, undefined) of
	center -> 
		center;
	_ ->
		String = get_option(pos, Opts, undefined),
		[X, Y] = get_integer_list(String, [-1, -1]),
		{X, Y}
	end.
	
get_date(Key, Opts, Default) ->
	case get_option(Key, Opts) of
	undefined ->
		Default;
	String ->
		try begin
			L = re:split(String, <<"[/\\-T\\: ]+">>),
			L0 = [list_to_integer(binary_to_list(X)) || X <- L],
			case L0 of
			[Mo, D, Y] ->
				{{Y, Mo, D}, {0, 0, 0}};
			[Y, Mo, D, H, M, S] ->
				{{Y, Mo, D}, {H, M, S}}
			end
		end catch
		_:_ -> Default
		end
	end.
		
get_integer_list(String, Default) ->
	try begin
		L = re:split(String, "[, ]+"),
		true = length(L) >= length(Default),
		[list_to_integer(binary_to_list(X)) || X <- L]
	end catch
	_:_ -> Default
	end.
%
get_control_record(Type, A, ExtendedAttributes) ->
	Id = get_atom(id, A),
	Label = get_option(label, A, <<"">>),
	Width = get_integer(width, A, -1),
	Height = get_integer(height, A, -1),
	Align = get_atom(align, A, left),
	Border = get_integer(border, A, 0),
	Fill = get_atom(fill, A, none),
	Enable = get_boolean(enable, A, true),
	Callbacks = get_callbacks(A),
	% IMPL: -define(CONTROL_ATTRS, id, ref, label, size = {-1, -1}, align = left, border = 0, fill = none, callbacks = []).
	list_to_tuple([Type, Id, undefined, Label, {Width, Height}, Align, Border, Fill, Enable, Callbacks] ++ ExtendedAttributes).
	
get_callbacks(List) ->
	[{K, binary_to_atom(V, utf8)} || {K, V} <- List, lists:member(K, ?GX_EVENTS)].

get_selected_item(List) ->
	get_selected_item(List, 0).
get_selected_item([{item, A, _}|T], Index) ->
	case get_boolean(selected, A, false) of
	true ->
		Index;
	false ->
		get_selected_item(T, Index + 1)
	end;
get_selected_item([], _) ->
	undefined.
	
get_text(Name, A, C) ->
	case get_option(Name, A) of
	undefined when length(C) > 0 ->
		[Text|_] = C,
		Text;
	undefined ->
		<<>>;
	Value ->
		Value
	end.

get_items(menu, List) ->
	[case element(1, X) of item -> setelement(1, X, menuitem); _ -> X end || X <- List];
get_items(toolbar, List) ->
	[case element(1, X) of item -> setelement(1, X, toolbutton); _ -> X end || X <- List];
get_items(tree, List) ->
	[case element(1, X) of item -> setelement(1, X, treeitem); _ -> X end || X <- List];
get_items(list, List) ->
	[X || {item, _, [X]} <- List];
get_items(radiobox, List) ->
	[X || {item, _, [X]} <- List];
get_items(choice, List) ->
	[X || {item, _, [X]} <- List];
get_items(combo, List) ->
	[X || {item, _, [X]} <- List];
get_items(checklist, List) ->
	[X || {item, _, [X]} <- List].

