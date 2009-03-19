%%
%%
%%
-module(gx).
-version("alpha").
-author('steve@simulacity.com').

-include("../include/gx.hrl").
-include("gx_events.hrl").
-include_lib("wx/include/wx.hrl").

-compile(export_all).

-export([start/2, create/1, create/3, destroy/2, get/2, set/3]).
-export([ % Components
	window/2, dialog/2, splashscreen/2,
	panel/2, box/2,
	menubar/2, menu/2, menuitem/2, separator/2,
	toolbar/2, toolitem/2, button/2, statusbar/2, 
	tabs/2, editor/2, alert/3 % many more...
]). 

% wxe.hrl is hidden so redefine...
-record(wx_ref, {ref, type, state=[]}).

%%
%% Core GUI startup and event loop
%%

%% Spawn a GUI process instance
start(Module, GUI) when is_atom(Module), is_list(GUI) ->
	gx_registry:start(),
	spawn_link(?MODULE, init, [Module, GUI]).

%% if we have a term definition, load the UI...
init(Module, [GxTerm|T]) when is_tuple(GxTerm) ->
	%% TODO: Keep this reminder until more than a single top level component
	%% for the UI can be created during init.
	io:format("[UNPARSED] ~p~n", [T]), 
	Window = gx:create(GxTerm),	
	% maybe here...
	wxTopLevelWindow:show(Window),
	% show triggers the gx:onload event
	do_init_handler(Module, Window, GxTerm),
	loop(Module, Window);
% ...or else load the xml file definition
init(Module, File) when is_list(File) ->
	{ok, Resource} = gx_registry:find_resource(File),
	{ok, TermList} = gx_xml:load(Resource),
	init(Module, TermList).

%% The main event loop for the GUI process instance
loop(Module, Frame) when is_atom(Module) ->
    receive 
	%% TODO: what if the window doesn't have an id/name?? 
	Evt = #wx{event=#wxClose{}} ->
		io:format("WXEVENT wxClose~n", []),
		case Evt#wx.userData of
		{GxName, Function} ->
			get_handler(Module, Evt),
			Module:Function(Frame, Evt);
		_ -> GxName = undefined
		end,
		destroy(Frame, GxName);
	Evt = #wx{} ->
		%% TODO: this is a hack -- improve it!
		Data = case Evt#wx.event of 
		{wxCommand, _Type, String, Integer, Long} -> 
			{String, Integer, Long};
		_ -> undefined
		end,
		{GxName, Callback} = get_handler(Module, Evt),
		
		Msg = #gx{id=GxName, type=Callback, event=Evt#wx.id, data=Data, wx=[Evt]},
		case Module:Callback(Frame, Msg) of 
		ok -> loop(Module, Frame);
		exit -> destroy(Frame, GxName);
		Value -> 
			io:format("INVALID CALLBACK ~p RETURNED ~p~n", [Callback, Value]),
			loop(Module, Frame)
		end;
	Evt ->
		io:format("INVALID EVENT ~p~n", [Evt]),
		loop(Module, Frame)
    after 1000 ->
		% check on externally updated files?
		loop(Module, Frame)
    end.

% Get a valid handler function for an event, if one is available
% It would probably be better to do all this at creation time, if possible
get_handler(Module, #wx{userData={GxName, GxHandler}}) ->
	Exports = Module:module_info(exports),
	case lists:member({GxHandler, 2}, Exports) of 
	true -> 
		{GxName, GxHandler};
	false -> 
		case lists:member({on_message, 2}, Exports) of
		true -> {GxName, on_message};
		false -> {error, no_callback_handler}
		end
	end;
get_handler(Module, E = #wx{id=Command}) when is_integer(Command) ->
	{GxName, GxHandler} = gx_registry:lookup_command(Command),
	get_handler(Module, E#wx{userData={GxName, GxHandler}}).

do_init_handler(Module, Parent, {_, Options, _}) ->
	case get_atom(onload, undefined, Options) of
	undefined -> ok;
	Function -> 
		Exports = Module:module_info(exports),
		case lists:member({Function, 2}, Exports) of 
		true ->
			GxName = get_atom(id, Options),
			Module:Function(Parent, {gx, GxName, Function, 0, [#wx{}]});
		false -> {error, no_callback_handler}
		end
	end.


%
% Core Utility Functions
% The following functions are to extract all the generic code from the
% component creators defined later.
%

%% Simple redirection: used to reduce code complexity inside component functions
get_atom(Key, Opts)              -> gx_registry:get_atom(Key, undefined, Opts).
get_atom(Key, Default, Opts)     -> gx_registry:get_atom(Key, Default, Opts).
get_boolean(Key, Opts)           -> gx_registry:get_boolean(Key, false, Opts).
get_boolean(Key, Default, Opts)  -> gx_registry:get_boolean(Key, Default, Opts).
get_integer(Key, Opts)           -> gx_registry:get_integer(Key, -1, Opts).
get_integer(Key, Default, Opts)  -> gx_registry:get_integer(Key, Default, Opts).
get_string(Key, Opts)            -> gx_registry:get_string(Key, "", Opts).
get_string(Key, Default, Opts)   -> gx_registry:get_string(Key, Default, Opts).
get_resource(Key, Opts)          -> gx_registry:get_resource(Key, Opts).
get_option(Key, Opts)            -> gx_registry:get_option(Key, undefined, Opts).
get_option(color, Default, Opts) -> gx_map:color(get_atom(color, Default, Opts));
get_option(Key, Default, Opts)   -> gx_registry:get_option(Key, Default, Opts).
%%
get_type(#wx_ref{type=WxType}) -> WxType.
is_top_level(Type) -> lists:member(Type, [frame, window, dialog, splashscreen]).

%% Extract any valid candidates for events from the component options
get_callbacks(Opts) ->
	Convert = fun(Name) ->
		case is_atom(Name) of
		true -> Name;
		false -> list_to_atom(Name)
		end
	end,
	[{X, Convert(Y)} || {X, Y} <- Opts, is_atom(X), lists:member(X, ?GX_EVENTS)].

%% Register a GX component
register(GxName, Component) ->
	gx_registry:add_component(GxName, Component).
%%
lookup(GxName) ->
	gx_registry:lookup_component(GxName).

%% the name for this function is somewhat badly chosen
%% as it's not the opposite of create...
destroy(Component, GxName) when is_tuple(Component), is_atom(GxName) ->	
	gx_registry:remove_component(Component, GxName),
	%% Assume parent_class is wxWindow...
	wxWindow:destroy(Component).

%% Register a GX command and the user-defined handler
command(GxName, {_WxType, WxMap}, GxHandler) ->
	[GxCallback] = map_callbacks(WxMap, [GxHandler], []),
	gx_registry:add_command(GxName, GxCallback).
	
%% TODO: Need to clarify naming in the code to reflect the exact differences 
%% between Handlers, Callbacks and Events 
%% Register/enable all (valid) user defined event handlers
events(GxName, Component, [{WxType, WxMap}|T], GxHandlers) ->
	GxCallbacks = map_callbacks(WxMap, GxHandlers, []),
	connect_callbacks(WxType, GxName, Component, GxCallbacks),
	events(GxName, Component, T, GxHandlers);
events(_, Component, [], _) -> 
	Component.

%% Associates the WX event directly with the user-defined handler
map_callbacks([WxMap|T], GxHandlers, Acc) ->
%	io:format("[MAPPING] ~p ~p~n", [WxMap, GxHandlers]),
	Acc1 = 
		case map_callback(WxMap, GxHandlers) of 
		undefined -> Acc;
		Callback -> [Callback|Acc]
		end,	
	map_callbacks(T, GxHandlers, Acc1);
map_callbacks([], _, Acc) ->
	Acc.
	
map_callback({GxEvent, WxEvent}, [{GxEvent, GxHandler}|_]) ->
	{WxEvent, GxHandler};
map_callback(WxMap, [_|T]) ->
	map_callback(WxMap, T);
map_callback(_, []) ->
	undefined. 

%% Finally, wire the callbacks from the WX component to the WXE server
connect_callbacks(WxType, GxName, Component, [{WxEvent, GxHandler}|T]) ->
	io:format("[EVENT   ] ~p '~p' {~p, ~p}~n", [Component#wx_ref.type, GxName, WxEvent, GxHandler]),
	WxType:connect(Component, WxEvent, [{userData, {GxName, GxHandler}}]),
	connect_callbacks(WxType, GxName, Component, T);
connect_callbacks(_, _, _, []) -> 
	ok.

%%
%% Generic property getter/setter
%% TODO: MAP GX properties to the WX function names

%% TODO: Property is currently directly(!!!) applied as the function name
get(GxName, Property) ->
	Component = lookup(GxName),
	case Component of 
	#wx_ref{type=Type} -> Type:Property(Component);
	_ -> undefined
	end.

%% TODO: Property is currently directly(!!!) applied as the function name
set(GxName, Property, Args) ->
	Component = lookup(GxName),
	case Component of 
	#wx_ref{type=Type} -> apply(Type, Property, [Component|Args]);
	_ -> undefined
	end.
	
%
% The Generic GX create/destroy functions
%

%%
create(Component, Parent, Opts) ->
	%io:format("[CREATE  ] ~p ~p ~p~n", [Component, Parent, Opts]),
	gx:Component(Parent, Opts).

%% trunk
create({GxType, Opts, Children}) ->
	case is_top_level(GxType) of
	true -> 
		GX = gx_registry:start(),
		wx:batch(fun() -> 
			%io:format("[CREATE  ] ~p ~p ~p~n", [type, GX, Opts]),
			Window = ?MODULE:GxType(GX, Opts),
			create_tree(Window, Children),
			
			%%%% TODO: this isn't working right for Frame furniture (statusbar, menu, etc) 
			%io:format("FRAME ~p children -> ~p~n", [get_type(Window), wxWindow:getChildren(Window)]),
			wxTopLevelWindow:fit(Window),
			wxTopLevelWindow:layout(Window),
			
			case get_pos(Opts) of
			center -> wxTopLevelWindow:center(Window);
			[X, Y] -> wxTopLevelWindow:move(Window, X, Y)
			end,
			Window 
		end);
	false ->
		{error, not_toplevel}
	end.

%% TODO: special cases of child elements 
%% Should implement in xml transform not here?
create_tree(Parent, [Component = {radiobox, _, _} | Rest]) ->
	create_choices(Parent, Component, Rest);
create_tree(Parent, [Component = {list, _, _} | Rest]) ->
	create_choices(Parent, Component, Rest);
create_tree(Parent, [Component = {checklist, _, _} | Rest]) ->
	create_choices(Parent, Component, Rest);
create_tree(Parent, [Component = {combo, _, _} | Rest]) ->
	create_choices(Parent, Component, Rest);
create_tree(Parent, [Component = {choice, _, _} | Rest]) ->
	create_choices(Parent, Component, Rest);
	
%% branch
create_tree(Parent, [{Component, Opts, Children} | Rest]) ->
	P = create(Component, Parent, Opts),
	%io:format("CREATE ~p~n", [P]),
	create_tree(P, Children),
	
	%% Important - layout doesn't work without this!
	%% this is necessary to ensure component sizes are correctly 
	%% propogated from child->parent
	case get_type(P) of 
	wxMenu -> ignore;
	wxMenuItem -> ignore;
	wxStatusBar -> wxWindow:fit(Parent);
	_ -> 
		case wxWindow:getSizer(P) of
		#wx_ref{ref = 0} -> ignore; % ? CORRECT ?
		Sizer -> wxSizer:fit(Sizer, Parent)
		end
	end,
	create_tree(Parent, Rest);
%% leaf
create_tree(Parent, [{Component, Opts} | Rest]) ->
	create(Component, Parent, Opts),
	%% at the leaf node, we don't have to worry about size propogation
	create_tree(Parent, Rest);
create_tree(Parent, []) ->
	%io:format("RETURN ~p~n", [Parent]), 
	Parent.

create_choices(Parent, {Component, Opts, Children}, Rest) ->
	Pred = fun(X) -> proplists:get_value(label, X) end,
	Labels = [Pred(Attr) || {item, Attr, []} <- Children],
	create(Component, Parent, [{choices, Labels} | Opts]),
	create_tree(Parent, Rest).

%%
create_sizer_flags(Opts) ->
	set_sizer_flags(wxSizerFlags:new(), Opts).
%%
set_sizer_flags(SizerFlags, Opts) ->
	Align = get_atom(align, left, Opts),
	case Align of 
	left   -> wxSizerFlags:left(SizerFlags);
	center -> wxSizerFlags:center(SizerFlags);
	right  -> wxSizerFlags:right(SizerFlags);
	_      -> ignore
	end,
	
	% TODO support border="10, 10, 0, 0" etc
	Border = get_integer(border, 0, Opts),
	wxSizerFlags:border(SizerFlags, ?wxALL, Border),
	
	Fill = get_boolean(fill, false, Opts),
	case Fill of % for now fill="true" means fill="both"
	true -> 
		wxSizerFlags:expand(SizerFlags),
		wxSizerFlags:proportion(SizerFlags, 1); 
	false -> ignore
	end,
	SizerFlags.

%%
update(Parent, Component, SizerFlags) ->
	%io:format("TYPE ~p SIZER ~p~n", [Parent, wxWindow:getSizer(Parent)]),
	case wxWindow:isTopLevel(Parent) of 
	true ->
		case wxWindow:getSizer(Component) of
		#wx_ref{ref=0} -> null_value;
		Sizer -> wxSizer:setSizeHints(Sizer, Parent)
		end;
	false ->
		Sizer = wxWindow:getSizer(Parent),
		wxSizer:add(Sizer, Component, SizerFlags)
		%wxSizer:fit(Sizer, Parent)	%'fit' should not really be called here if possible
	end.

%% TODO: use this from funs for getting the pos, color, size integer lists.
get_pos(Opts) ->
	case get_atom(pos, Opts) of
	center -> center;
	_ ->
		String = get_string(pos, Opts),
		get_integer_list(String, [-1, -1])
	end.

get_integer_list(String, Default) ->
	try begin
		L = re:split(String, "[, ]+"),
		true = length(L) >= length(Default),
		[list_to_integer(binary_to_list(X)) || X <- L]
	end catch
	_:_ -> Default
	end.

%% Allow shorthand tag of 'item' inside menus and toolbars
item(Parent = #wx_ref{type=wxMenu}, Opts) ->	
	menuitem(Parent, Opts);
item(Parent = #wx_ref{type=wxToolBar}, Opts) ->	
	toolitem(Parent, Opts).

%%%
%%% GX/WX Components
%%%

%%
%% Top Level Components
%%
% alias this away
frame(Parent, Opts) -> window(Parent, Opts).

%% Opts = [{title, String}|{width, Integer}|{height, Integer}|{icon, Path}]
window(Parent = #wx_ref{type=wx}, Opts) ->
	GxName = get_atom(id, Opts),
	Title = get_string(title, "Untitled", Opts),
	X = get_integer(width, Opts),
	Y = get_integer(height, Opts), 
	Frame = wxFrame:new(Parent, -1, Title),
	Icon = get_resource(icon, Opts),
	wxFrame:setIcon(Frame, Icon),	

% make into a subpanel to be able to use the
% "special" top level window sizer??	 
	case X > -1 orelse Y > -1 of
	true ->
		Sizer = wxBoxSizer:new(?wxVERTICAL),
		wxSizer:setMinSize(Sizer, X, Y),
		% how to apply these???
		SizerFlags = wxSizerFlags:new(),
		wxSizerFlags:expand(SizerFlags),
		wxFrame:setSizer(Frame, Sizer),
		wxSizer:setSizeHints(Sizer, Frame);
	false ->
		ok
	end,
	
	Callbacks = get_callbacks(Opts),
	%% NOTE: Should add inherited event mapping macros too, e.g. CLOSE!!!?
	gx:events(GxName, Frame, [?GX_WINDOW_EVENTS], Callbacks),
	gx:register(GxName, Frame).

%% TODO: Not fully implemented
%% Opts = [{title, String}|{width, Integer}|{height, Integer}|{icon, Path}]
dialog(Parent = #wx_ref{type=wx}, Opts) ->
	GxName = get_atom(id, Opts),
	Title = get_string(title, "Untitled", Opts),
	X = get_integer(width, Opts),
	Y = get_integer(height, Opts), 
	Dialog = wxDialog:new(Parent, -1, Title, [{size, {X, Y}}]),
	Icon = get_resource(icon, Opts),
	wxDialog:setIcon(Dialog, Icon),	

	Callbacks = get_callbacks(Opts),
	%% NOTE: Should add inherited event mapping macros too, e.g. CLOSE!!!?
	gx:events(GxName, Dialog, [?GX_WINDOW_EVENTS], Callbacks),
	gx:register(GxName, Dialog).

%%
%% Containers/Layouts
%%

%% TODO: Not fully implemented
x_window(Parent, Opts) -> 
	GxName = get_atom(id, Opts),
	X = get_integer(width, 200, Opts),
	Y = get_integer(height, 200, Opts),	
	Window = wxWindow:new(Parent, -1, [{size, {X, Y}}]),
	
	Callbacks = get_callbacks(Opts),
	gx:events(GxName, Window, [?GX_WINDOW_EVENTS], Callbacks),
	gx:register(GxName, Window).

%%
panel(Parent, Opts) ->
	GxName = get_atom(id, Opts),
	Layout = get_atom(layout, column, Opts),
	Panel = wxPanel:new(Parent),
	X = get_integer(width, Opts),
	Y = get_integer(height, Opts),
	
	case get_option(color, undefined, Opts) of
		undefined -> proceed;
		Color -> wxPanel:setOwnBackgroundColour(Panel, Color)
	end,
	
	Sizer = case Layout of
	column -> wxBoxSizer:new(?wxVERTICAL);
	vertical -> wxBoxSizer:new(?wxVERTICAL);
	row -> wxBoxSizer:new(?wxHORIZONTAL);
	horizontal -> wxBoxSizer:new(?wxHORIZONTAL);
	grid ->
		Columns = get_integer(cols, 0, Opts),
		Rows = get_integer(rows, 0, Opts),
		Padding = get_integer(padding, 0, Opts),
		if 
			Columns > 0, Rows > 0 -> wxGridSizer:new(Rows, Columns, Padding, Padding);
			Columns =< 0, Rows > 0 -> wxGridSizer:new(Rows, 2, Padding, Padding);
			true -> wxGridSizer:new(2, [{hgap, Padding}, {vgap, Padding}])
		end;
	_ -> 
		undefined
	end,
	
	wxSizer:setMinSize(Sizer, {X, Y}),
	wxPanel:setSizer(Panel, Sizer),
	
	SizerFlags = create_sizer_flags(Opts),	
	update(Parent, Panel, SizerFlags),
	gx:register(GxName, Panel).

%%
tabs(Parent, Opts) ->
	GxName = get_atom(id, Opts),
	Notebook = wxNotebook:new(Parent, -1, []),
	Sizer = wxBoxSizer:new(?wxHORIZONTAL),
	wxNotebook:setSizer(Notebook, Sizer),

	SizerFlags = create_sizer_flags(Opts), 
	update(Parent, Notebook, SizerFlags),
	gx:register(GxName, Notebook).

%%
tab(Parent, Opts) ->
	GxName = get_atom(id, Opts),
	Label = get_string(label, Opts),
	Panel = wxPanel:new(), 
	Sizer = wxBoxSizer:new(?wxVERTICAL),
	wxPanel:setSizer(Panel, Sizer),
	wxNotebook:addPage(Parent, Panel, Label),
	
	SizerFlags = create_sizer_flags(Opts),
	update(Parent, Panel, SizerFlags),
	gx:register(GxName, Panel).

%%
%% Basic Controls
%%

%% Opts = [{label, String}] 
button(Parent, Opts) ->
	GxName = get_atom(id, Opts),
	Label = get_string(label, "OK", Opts),
	Button = wxButton:new(Parent, -1, [{label, Label}]),
	X = get_integer(width, -1, Opts),
	Y = get_integer(height, -1, Opts),
	wxButton:setMinSize(Button, {X, Y}),
	wxButton:setName(Button, atom_to_list(GxName)),
	
	
	Callbacks = get_callbacks(Opts),
	gx:events(GxName, Button, [?GX_BUTTON_EVENTS], Callbacks),
	SizerFlags = create_sizer_flags(Opts),
	update(Parent, Button, SizerFlags),
	gx:register(GxName, Button).

%%
checkbox(Parent, Opts) ->
	GxName = get_atom(id, Opts),
	Label = get_string(label, "(undefined)", Opts),
	CheckBox = wxCheckBox:new(Parent, -1, Label, []),
	
	Callbacks = get_callbacks(Opts),
	gx:events(GxName, CheckBox, [?GX_CHECKBOX_EVENTS], Callbacks),
	SizerFlags = create_sizer_flags(Opts),
	update(Parent, CheckBox, SizerFlags),	
	gx:register(GxName, CheckBox).
	
%%
radiobutton(Parent, Opts) ->
	GxName = get_atom(id, Opts),
	Label = get_string(label, "(undefined)", Opts),
	
	RadioButton = wxRadioButton:new(Parent, -1, Label, []),
	
	Callbacks = get_callbacks(Opts),
	gx:events(GxName, RadioButton, [?GX_RADIOBUTTON_EVENTS], Callbacks),
	SizerFlags = create_sizer_flags(Opts),
	update(Parent, RadioButton, SizerFlags),
	gx:register(GxName, RadioButton).

%%
radiobox(Parent, Opts) ->
	GxName = get_atom(id, Opts),
	Label = get_string(label, "", Opts),
	Choices = get_option(choices, [], Opts),
	
	RadioBox = wxRadioBox:new(Parent, -1, Label, {-1, -1}, {-1, -1}, Choices),
	
	Callbacks = get_callbacks(Opts),
	gx:events(GxName, RadioBox, [?GX_RADIOBOX_EVENTS], Callbacks),
	SizerFlags = create_sizer_flags(Opts),
	update(Parent, RadioBox, SizerFlags),	
	gx:register(GxName, RadioBox).

%%
list(Parent, Opts) ->
	GxName = get_atom(id, Opts),
	_Label = get_string(label, "", Opts),
	Choices = get_option(choices, [], Opts),
	
	ListBox = wxListBox:new(Parent, -1, [{choices, Choices}]),
	
	Callbacks = get_callbacks(Opts),
	gx:events(GxName, ListBox, [?GX_LISTBOX_EVENTS], Callbacks),
	SizerFlags = create_sizer_flags(Opts),
	update(Parent, ListBox, SizerFlags),	
	gx:register(GxName, ListBox).

%%
checklist(Parent, Opts) ->
	GxName = get_atom(id, Opts),
	_Label = get_string(label, "", Opts),
	Choices = get_option(choices, [], Opts),
	
	CheckListBox = wxCheckListBox:new(Parent, -1, [{choices, Choices}]),
	
	Callbacks = get_callbacks(Opts),
	gx:events(GxName, CheckListBox, [?GX_CHECKLISTBOX_EVENTS], Callbacks),
	SizerFlags = create_sizer_flags(Opts),
	update(Parent, CheckListBox, SizerFlags),	
	gx:register(GxName, CheckListBox).

%% 
choice(Parent, Opts) ->
	GxName = get_atom(id, Opts),
	_Label = get_string(value, "", Opts),
	Choices = get_option(choices, [], Opts),
	
	Choice = wxChoice:new(Parent, -1, [{choices, Choices}]),
		%{style, ?wxCB_DROPDOWN}
	
	Callbacks = get_callbacks(Opts),
	gx:events(GxName, Choice, [?GX_CHOICE_EVENTS], Callbacks),
	SizerFlags = create_sizer_flags(Opts),
	update(Parent, Choice, SizerFlags),	
	gx:register(GxName, Choice).

%%
combo(Parent, Opts) ->
	GxName = get_atom(id, Opts),
	Value = get_string(value, "", Opts),
	Choices = get_option(choices, [], Opts),
	
	ComboBox = wxComboBox:new(Parent, -1, [{value, Value}, {choices, Choices}]),
	 %{style, ?wxCB_DROPDOWN}
	 
	Callbacks = get_callbacks(Opts),
	gx:events(GxName, ComboBox, [?GX_COMBOBOX_EVENTS], Callbacks),
	SizerFlags = create_sizer_flags(Opts),
	update(Parent, ComboBox, SizerFlags),	
	gx:register(GxName, ComboBox).

%
spinner(Parent, Opts) ->
	GxName = get_atom(id, Opts),
	Start = get_integer(start, 0, Opts),
	End = get_integer('end', 10, Opts),
	Value = get_integer(value, 5, Opts),
	Text = get_string(text, "", Opts),
	_Wrap = get_boolean(wrap, false, Opts),
	Style = 
	case get_boolean(wrap, false, Opts) of
	true -> ?wxSP_WRAP;
	false -> 0
	end,
	Spinner = wxSpinCtrl:new(Parent, [{style, Style}, {min, Start}, {max, End}, 
		{initial, Value}, {value, Text}]),
	
	Callbacks = get_callbacks(Opts),
	gx:events(GxName, Spinner, [?GX_SPINNER_EVENTS], Callbacks),
	SizerFlags = create_sizer_flags(Opts),
	update(Parent, Spinner, SizerFlags),	
	gx:register(GxName, Spinner).

%
text(Parent, Opts) ->
	GxName = get_atom(id, Opts),
	Label = get_string(label, Opts),
	Wrap = get_integer(wrap, -1, Opts),
	StaticText = wxStaticText:new(Parent, -1, Label, []),
	wxStaticText:wrap(StaticText, Wrap),
	
	SizerFlags = create_sizer_flags(Opts),
	update(Parent, StaticText, SizerFlags),
	gx:register(GxName, StaticText).
	
%%
line(Parent, Opts) ->
	GxName = get_atom(id, Opts),
	_Label = get_string(label, Opts),
	Orientation = 
	%% Note: Why StaticLine doesn't use ?wxHORIZONTAL and ?wxVERTICAL like every
	%% other component is a total mystery to me
	case get_atom(orientation, horizontal, Opts) of
	vertical -> ?wxLI_VERTICAL;
	_ -> ?wxLI_HORIZONTAL
	end,
	StaticLine = wxStaticLine:new(Parent, [{size, {100,2}}, {style, Orientation}]),

	SizerFlags = create_sizer_flags(Opts),	
	update(Parent, StaticLine, SizerFlags),	
	gx:register(GxName, StaticLine).

%% 
box(Parent, Opts) ->
	GxName = get_atom(id, Opts),
	Label = get_string(label, Opts),
	X = get_integer(width, -1, Opts),
	Y = get_integer(height, -1, Opts),
	
	%% NOTE: wxStaticBox must be added as a first sibling, and not used as
	%% the parent or wxWidgets will *crash* on exit. Thus we enclose the
	%% box in a panel and add in the StaticBox as the first child.	
	Panel = wxPanel:new(Parent),
	Box = wxStaticBox:new(Panel, -1, Label, [{size, {X, Y}}]),
	Sizer = wxBoxSizer:new(?wxVERTICAL),
	BoxSizer = wxStaticBoxSizer:new(Box, ?wxVERTICAL),
	wxSizer:add(BoxSizer, Sizer),
	wxPanel:setSizer(Panel, BoxSizer),
	
	SizerFlags = create_sizer_flags(Opts),
	update(Parent, Panel, SizerFlags),	
	gx:register(GxName, Panel).

%%
slider(Parent, Opts) ->
	GxName = get_atom(id, Opts),
	Start = get_integer(start, 0, Opts),
	End = get_integer('end', 10, Opts),
	Value = get_integer(value, 5, Opts),
	
	Ticks = 
	case get_boolean(ticks, false, Opts) of
	true -> ?wxSL_AUTOTICKS;
	false -> 0
	end,
	Labels = 
	case get_boolean(labels, false, Opts) of
	true -> ?wxSL_LABELS;
	false -> 0
	end,
	
	Style = ?wxSL_HORIZONTAL bor ?wxSL_BOTTOM bor Ticks bor Labels,
	Slider = wxSlider:new(Parent, -1, Value, Start, End, [{style, Style}]),
	
	Callbacks = get_callbacks(Opts),
	gx:events(GxName, Slider, [?GX_SLIDER_EVENTS], Callbacks),
	SizerFlags = create_sizer_flags(Opts),
	update(Parent, Slider, SizerFlags),	
	gx:register(GxName, Slider).

%%
%% Frame furniture
%%

%%
menubar(Parent = #wx_ref{type=wxFrame}, Opts) ->
	GxName = get_atom(id, Opts),
    MenuBar = wxMenuBar:new(),
	wxFrame:setMenuBar(Parent, MenuBar),
	%% TODO: there are more menubar events to implement
    wxFrame:connect(Parent, command_menu_selected),
	gx:register(GxName, MenuBar).

%% Opts = [{label, String}]
menu(Parent, Opts) ->
	GxName = get_atom(id, Opts),
	Label = get_string(label, "", Opts),
	Menu = wxMenu:new(),
	wxMenuBar:append(Parent, Menu, Label),	
	gx:register(GxName, Menu).

%% Opts = [{label, String} | {enable, Bool} | {command, Integer}]
menuitem(Parent = #wx_ref{type=wxMenu}, Opts) -> 
	GxName = get_atom(id, Opts),
	Label = get_string(label, Opts),
	Type = get_atom(type, normal, Opts),
	
	%% TODO: can ignore multiple (but illegal) callbacks here... is thsi VALID???
	Command = case get_callbacks(Opts) of
	[Callback|_] -> gx:command(GxName, ?GX_MENU_COMMAND, Callback);
	[] -> ?wxID_NONE
	end,
	Item = case Type of 
	radio    -> wxMenu:appendRadioItem(Parent, Command, Label);
	checkbox -> wxMenu:appendCheckItem(Parent,  Command, Label);
	normal   -> wxMenu:append(Parent, Command, Label);
	_        -> {error, invalid_menu_item}
	end,
	Checked = get_boolean(checked, false, Opts),
	wxMenuItem:check(Item, [{check, Checked}]),
	Enabled = get_boolean(enable, true, Opts),
	wxMenuItem:enable(Item, [{enable, Enabled}]),
	gx:register(GxName, Item).

%% also for toolbar later...
separator(Parent = #wx_ref{type=wxMenu}, _Opts) ->
	wxMenu:appendSeparator(Parent).

%%
toolbar(Parent, Opts) ->
	Name = get_atom(id, Opts),
	ToolBar = wxFrame:createToolBar(Parent, []),
	gx:register(Name, ToolBar).

%% Opts = [{label, String} | {icon, Path}]
toolitem(_Parent = #wx_ref{type=wxToolBar}, Opts) ->
	_Label = get_string(label, "", Opts),
	_Icon = get_resource(icon, Opts).
%% NOTE: doesn't work - need to understand more about wx realize cpp call
%	wxToolBar:addTool(Parent, -1, Label, Icon),
%	wxToolBar:realize(Parent).
 
%% Opts = [{text, String}]
statusbar(Parent, Opts) ->
	GxName = get_atom(id, Opts),
	StatusBar = wxFrame:createStatusBar(Parent, []), 
	Text = get_option(text, "", Opts),
	wxFrame:setStatusText(Parent, Text, []),
	gx:register(GxName, StatusBar).

%%
%% Advanced Controls
%%

%% Opts = [{label, String}]
editor(Parent, Opts) ->
	GxName = get_atom(id, Opts),
	Value = get_option(value, "Untitled", Opts),
	X = get_option(width, 200, Opts),
	Y = get_option(height, 200, Opts),
	
	Editor = wxTextCtrl:new(Parent, -1, [{value, Value}, {size, {X, Y}}, {style, ?wxTE_MULTILINE}]),
	
	SizerFlags = create_sizer_flags(Opts),
	update(Parent, Editor, SizerFlags),

	gx:register(GxName, Editor),
	Editor.


%%
%% System Dialogs
%%

%% wxErlang BUG(?): The required style macros are missing from wxErlang
%% From /include/wx/generic/splash.h
%% #define wxSPLASH_CENTRE_ON_PARENT   0x01
%% #define wxSPLASH_CENTRE_ON_SCREEN   0x02
%% #define wxSPLASH_NO_CENTRE          0x00
%% #define wxSPLASH_TIMEOUT            0x04
%% #define wxSPLASH_NO_TIMEOUT         0x00
splashscreen(Parent, Opts) ->
	Timeout = get_integer(timeout, 5000, Opts),
	Image = get_resource(image, Opts),
	wxSplashScreen:new(Image, 16#02 bor 16#04, Timeout, Parent, -1).


%% TODO: generalize these remove from registry on close!
%%
alert(Parent, Message, Opts) ->
	Name = get_atom(id, Opts),
	Caption = get_string(title, "", Opts),
	Dialog = wxMessageDialog:new(Parent, Message,
		[{style, ?wxOK bor ?wxICON_INFORMATION}, {caption, Caption}]),
	gx:register(Name, Dialog),
	wxDialog:centreOnParent(Dialog),
    wxDialog:showModal(Dialog),
    wxDialog:destroy(Dialog). 

%%
colordialog(Parent, Opts) ->
	Name = get_atom(id, Opts),
	ColorDialog = wxColourDialog:new(Parent),
	gx:register(Name, ColorDialog),
	case wxColourDialog:showModal(ColorDialog) of
	?wxID_OK ->
		ColorData = wxColourDialog:getColourData(ColorDialog),
		Color = wxColourData:getColour(ColorData);
%		Data = {wxFont:getFamily(Font), wxFont:getFaceName(Font), wxFont:getPointSize(Font), wxFont:getWeight(Font)};
	_ -> Color = undefined
	end,
	wxColourDialog:destroy(ColorDialog),
	Color.

%%
filedialog(Parent, Opts) ->
	Name = get_atom(id, Opts),
	FileDialog = wxFileDialog:new(Parent),
	gx:register(Name, FileDialog),
    wxFileDialog:showModal(FileDialog),
	Filename = wxFileDialog:getFilename(FileDialog),
	wxFileDialog:destroy(FileDialog),
	Filename.

fontdialog(Parent, Opts) ->
	Name = get_atom(id, Opts),
	FontDialog = wxFontDialog:new(Parent),
	gx:register(Name, FontDialog),
	case wxFontDialog:showModal(FontDialog) of
	?wxID_OK ->
		FontData = wxFontDialog:getFontData(FontDialog),
		Font = wxFontData:getChosenFont(FontData),
		Data = {wxFont:getFamily(Font), wxFont:getFaceName(Font), wxFont:getPointSize(Font), wxFont:getWeight(Font)};
	_ -> Data = undefined
	end,
	wxFontDialog:destroy(FontDialog),
	Data.
