%%
%% GX Framework
%% Copyright 2009 <steven.charles.davis@gmail.com>. All rights reserved.
%% LICENSE: The correct license type has not yet been determined.
%%
-module(gx).
-version("0.1").
-author('steve@simulacity.com').

-include("../include/gx.hrl").
-include_lib("wx/include/wx.hrl").
%% Original Definiton of wx_ref is in 'hidden' header wxe.hrl
-record(wx_ref, {ref, type, state=[]}).

-compile(export_all).
-export([start/2, create/1, create/3, destroy/2]).
-export([ % Components
	window/2, dialog/2, splashscreen/2,
	panel/2, box/2,
	menubar/2, menu/2, menuitem/2, separator/2,
	toolbar/2, toolitem/2, button/2, statusbar/2, 
	tabs/2, editor/2, alert/3 % many more...
]). 


%% TODO: remove?
-define(GX_WINDOW_EVENTS, [{onunload, close_window}]).

%% TODO:gx
%% Regression: Window closing no longer reliably captured, so registry leaks

% Convenience
run(File) ->
	gx_runner:start(File).

%%
%% Core GUI startup and event loop
%%
start() ->
	gx_registry:start().
stop() ->
	gx_registry:stop().
	
%% Spawn a GUI process instance
start(Module, GUI) when is_atom(Module), is_list(GUI) ->
	gx_registry:start(),
	spawn_link(?MODULE, init, [Module, GUI]).

%% if we have a term definition, load the UI...
init(Module, [GxTerm|T]) when is_tuple(GxTerm) ->
	case T of 
	[] -> ok;
	%% TODO: Keep the following reminder until more than a single top level component
	%% for the UI can be created during init.
	T -> error_logger:warning_report([{unparsed_components, T}])
	end,
	Window = gx:create(GxTerm),	
	% SHOW - maybe here... maybe not
	wxTopLevelWindow:show(Window),
	% trigger the gx:onload event
	do_init_handler(Module, Window, GxTerm),
	% do main loop
	loop(Module, Window);
% ...or else load the xml file definition
init(Module, File) when is_list(File) ->
	{ok, Resource} = gx_registry:find_resource(File),
	{ok, TermList} = gx_xml:load(Resource),
	init(Module, TermList).


%% SEVERE HACKAGE ALERT FROM HERE...

%% The main event loop for the GUI process instance
loop(Module, Window) when is_atom(Module) ->
    receive 
	%% DEBUG
	Evt = #wx{} ->
		%io:format("wxEvent: ~p~n", [Evt]), 
		%%% TODO: IN BAD NEED OF REVIEW AND MORE WORK!
		Handler = 
			case Evt#wx.event of 
			#wxClose{} when is_tuple(Evt#wx.userData) ->
				Evt#wx.userData;
			#wxClose{} ->
				{gx, exit};
			#wxCommand{} when is_tuple(Evt#wx.userData) ->
				Evt#wx.userData;
			#wxCommand{} -> % menu events
				gx_registry:lookup_command(Evt#wx.id)
			end,
		
		case validate_handler(Module, Handler) of
		{gx, exit} ->
			destroy(Window, undefined);
		{GxName, GxEvent, Callback} ->
			GxEvt = translate_event(Evt, GxName, GxEvent, Callback),
			case Module:Callback(Window, GxEvt) of 
			exit -> 
				destroy(Window, GxName);
			ok -> 
				loop(Module, Window);
			Value -> 
				error_logger:error_report([
					{invalid_callback, Callback}, {value, Value}]),
				loop(Module, Window)
			end;
		undefined ->
			error_logger:error_report([
				{invalid_handler, Handler}]),
			loop(Module, Window)
		end;
	Evt = #gx{} ->  %% NOTE: EXPERIMENATAL!
		{gx, call, F, A} = Evt,
		apply(Module, F, A),
		loop(Module, Window);
	Evt ->
		error_logger:error_report([{invalid_event, Evt}]),
		loop(Module, Window)
    after 1000 ->
		% check on externally updated files?
		loop(Module, Window)
    end.

% Get a valid handler function for an event, if one is available
% It would probably be better to do all this at creation time, if possible
validate_handler(_Module, Handler = {gx, _}) ->
	Handler;
validate_handler(Module, {GxName, GxEvent, GxHandler}) ->
	Exports = Module:module_info(exports),
	case lists:member({GxHandler, 2}, Exports) of 
	true -> 
		{GxName, GxEvent, GxHandler};
	false -> 
		case lists:member({on_message, 2}, Exports) of
		true -> {GxName, GxEvent, on_message};
		false -> undefined
		end
	end.

%% 
translate_event(Evt = #wx{}, GxName, GxEvent, _GxCallback) ->
	WxRef = Evt#wx.obj,	
	WxData = 
		case Evt#wx.event of
		Cmd = #wxCommand{} -> [Cmd#wxCommand.cmdString, 
			Cmd#wxCommand.commandInt, Cmd#wxCommand.extraLong];
		_ -> []
		end,
	Ident = case GxName of
		undefined -> Evt#wx.id;
		_ -> GxName
		end,
	#gx{id=Ident, 
		type=gx_map:gtype(WxRef#wx_ref.type), 
		event=GxEvent, 
		data=WxData,
		user=[],
		wx=[Evt#wx.id]}. % wx=Evt}.
				
%
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
%% BUG:should be -> get_resource(Type, Key, Opts)
get_resource(Key, Opts)          -> gx_registry:get_resource(Key, Opts).
get_option(Key, Opts)            -> gx_registry:get_option(Key, undefined, Opts).
get_option(color, Default, Opts) -> gx_map:color(get_atom(color, Default, Opts));
get_option(Key, Default, Opts)   -> gx_registry:get_option(Key, Default, Opts).

%% Extract any valid candidates for event handlers from the component options
get_callbacks(Opts) ->
	% TODO: Convert copes with string input from gxml...
	% I not sure this is in the right place to do this but also
	% I am not sure that gxml should know which are valid as 
	% that would add an unnecessary dependency, perhaps
	Convert = fun(Name) ->
		case is_atom(Name) of
		true -> Name;
		false -> list_to_atom(Name)
		end
	end,
	[{X, Convert(Y)} || {X, Y} <- Opts, is_atom(X), lists:member(X, ?GX_EVENTS)].

%%
is_top_level(GxType) ->
	WxType = gx_map:wtype(GxType),
	gx_map:instance_of(WxType, wxTopLevelWindow).

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
	%% TDDO: Assumes the parent class is wxWindow...
	error_logger:info_report([
		{process, self()},
		{destroyed, Component}
	]),
	wxWindow:destroy(Component).

%% Register a GX command and the user-defined handler
set_command(GxName, WxEvents, {GxEvent, GxHandler}) ->
	[GxCallback] = [{WxEvent, GxHandler} || {GxEvent1, WxEvent} <- WxEvents, GxEvent == GxEvent1],
	gx_registry:add_command(GxName, GxCallback).
	
%% TODO: Need to clarify naming in the code to reflect the exact differences 
%% between Handlers, Callbacks and Events 
%% Register/enable all (valid) user defined event handlers
set_events(GxName, Component, WxEvents, GxHandlers) ->
	GxCallbacks = [
		{GxEvent, WxEvent, GxHandler} || 
		{GxEvent, WxEvent} <- WxEvents, 
		{GxEvent1, GxHandler} <- GxHandlers, 
		GxEvent == GxEvent1
	],
	connect_callbacks(Component#wx_ref.type, GxName, Component, GxCallbacks).

%% Finally, wire the callbacks from the WX component to the WXE server
connect_callbacks(WxType, GxName, Component, [{GxEvent, WxEvent, GxHandler}|T]) ->
	WxType:connect(Component, WxEvent, [{userData, {GxName, GxEvent, GxHandler}}]),
	connect_callbacks(WxType, GxName, Component, T);
connect_callbacks(_, _, _, []) -> 
	ok.

%%
%% Generic property getter/setter
%% WARNING: these include experimental remote code!
-record(gx_ref, {pid, id, wx}).

%% Extended use case: gx:read(listbox, selected, [{item, 0}]).
%% read/2
read(Gx = #gx_ref{}, Property) ->
	Gx#gx_ref.pid ! {Gx, Property};
read(IdOrRef, Property) -> read(IdOrRef, Property, []).
% read/3
read(GxName, Property, Opts) when is_tuple(Opts) ->
	read(GxName, Property, [Opts]);
read(GxName, Property, Opts) when is_atom(GxName) ->
	read(lookup(GxName), Property, Opts);
read(WxRef = #wx_ref{}, Property, Opts) when is_atom(Property), is_list(Opts) ->
	gx_map:get(WxRef, Property, Opts).

%% Extended use case: gx:config(listbox, [{selected, [{item, 0}]}, {style, blah}]).
% config/2
config(Gx = #gx_ref{}, Property) ->
	Gx#gx_ref.pid ! {Gx, Property};
config(GxName, Properties) when is_atom(GxName) -> 
	config(lookup(GxName), Properties);
config(WxRef, Properties) when is_tuple(Properties) ->
	config(WxRef, [Properties]);
config(WxRef = #wx_ref{}, Properties) when is_list(Properties) ->
	[gx_map:set(WxRef, Property, Opts) || {Property, Opts} <- Properties].

%
% The Generic GX create/destroy functions
%

%% create/3
create(Component, Parent, Opts) ->
	gx:Component(Parent, Opts).

%% trunk
create({GxType, Opts, Children}) ->
	case is_top_level(GxType) of
	true -> 
		Gx = gx_registry:start(),
		wx:batch(fun() -> 
			Window = ?MODULE:GxType(Gx, Opts),
			create_tree(Window, Children),
			
			%%%% TODO: this isn't working right for Frame furniture (statusbar, menu, etc) 
			wxTopLevelWindow:fit(Window),
			wxTopLevelWindow:layout(Window),
			
			case get_pos(Opts) of
			center -> wxTopLevelWindow:center(Window);
			[X, Y] -> wxTopLevelWindow:move(Window, X, Y)
			end,
			gx_registry:report_info(get_atom(id, Opts), Window),
			Window 
		end);
	false ->
		{error, not_toplevel}
	end.
	
%% branch 
create_tree(Parent, [{Type, Opts, Children} | Rest]) ->
	P = create(Type, Parent, Opts),
	create_tree(P, Children),
	%% IMPL: Important - layout doesn't work without the following!
	%% This is necessary to ensure component sizes are correctly 
	%% propogated from child->parent
	case P#wx_ref.type of 
	wxMenu -> ignore;
	wxMenuItem -> ignore;
	wxTreeItemId -> ignore;
	wxTreeCtrl -> 
		Root = wxTreeCtrl:getRootItem(P),
		wxTreeCtrl:expand(P, Root);
	wxStatusBar -> wxWindow:fit(Parent); %% no effect DARN IT!
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
	create_tree(Parent, Rest);
create_tree(Parent, []) ->
	Parent.


%% 
%% Generic Utility Functions for Components
%%
create_sizer_flags(Opts) ->
	set_sizer_flags(wxSizerFlags:new(), Opts).
%%
set_sizer_flags(SizerFlags, Opts) ->
	% TODO: support border="10, 10, 0, 0" etc
	Border = get_integer(border, 0, Opts),
	wxSizerFlags:border(SizerFlags, ?wxALL, Border),

	Align = get_atom(align, left, Opts),
	case Align of 
	left   -> wxSizerFlags:left(SizerFlags);
	center -> wxSizerFlags:center(SizerFlags);
	right  -> wxSizerFlags:right(SizerFlags);
	_      -> ignore
	end,
	
	Fill = get_boolean(fill, false, Opts),
	case Fill of % for now fill="true" means fill="both"
	true -> 
		wxSizerFlags:expand(SizerFlags),
		wxSizerFlags:proportion(SizerFlags, 1); 
	false -> ignore
	end,
	SizerFlags.

%% TODO: try to refactor this out entirely
update(Parent, Component, SizerFlags) ->
	case wxWindow:isTopLevel(Parent) of 
	true ->
		case wxWindow:getSizer(Component) of
		#wx_ref{ref=0} -> ok; 
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

%%%
%%% GX/WX Components
%%%

%% Allow shorthand tag of 'item' inside menus and toolbars
item(Parent = #wx_ref{type=wxMenu}, Opts) ->	
	menuitem(Parent, Opts);
item(Parent = #wx_ref{type=wxToolBar}, Opts) ->	
	toolitem(Parent, Opts).

%%
%% Top Level Components
%%
% Alias this away - REMOVE LATER?
frame(Parent, Opts) -> window(Parent, Opts).

%% Opts = [{title, String}|{width, Integer}|{height, Integer}|{icon, Path}] etc
window(Parent = #wx_ref{type=wx}, Opts) ->
	GxName = get_atom(id, Opts),
	Title = get_string(title, "Untitled", Opts),
	Frame = wxFrame:new(Parent, -1, Title),
	
	Icon = get_resource(icon, Opts),
	wxFrame:setIcon(Frame, Icon),	

	X = get_integer(width, Opts),
	Y = get_integer(height, Opts), 
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
	wxFrame:connect(Frame, close_window, []),
	set_events(GxName, Frame, ?GX_WINDOW_EVENTS, Callbacks),
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
	wxFrame:connect(Dialog, close_window, []),
	set_events(GxName, Dialog, ?GX_WINDOW_EVENTS, Callbacks),
	gx:register(GxName, Dialog).

%%
%% Containers/Layouts
%%

%% TODO: Do you ever really need to create a generic wxWindow?
%% I suspect this should be entirely removed
wx_window(Parent, Opts) -> 
	GxName = get_atom(id, Opts),
	X = get_integer(width, 200, Opts),
	Y = get_integer(height, 200, Opts),	
	Window = wxWindow:new(Parent, -1, [{size, {X, Y}}]),
	
	Callbacks = get_callbacks(Opts),
	set_events(GxName, Window, ?GX_WINDOW_EVENTS, Callbacks),
	gx:register(GxName, Window).

%% Equivalent to what GS calls a 'frame'
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

%% TODO: Note that most code is shared and could be refactored out

%% Generic control
create_control(Parent = #wx_ref{}, Create, StyleFlags, Events, Opts) ->
	Component = Create(),
	GxName = get_atom(id, Opts),	
	wxControl:setName(Component, atom_to_list(GxName)),
	
	X = get_integer(width, -1, Opts),
	Y = get_integer(height, -1, Opts),
	wxControl:setMinSize(Component, {X, Y}),
		
	Callbacks = get_callbacks(Opts),
	set_events(GxName, Component, Events, Callbacks),
	
	SizerFlags = create_sizer_flags(Opts),
	case gx_map:instance_of(Parent, wxTopLevelWindow) of 
	true ->
		case wxWindow:getSizer(Component) of
		#wx_ref{ref=0} -> ok; 
		Sizer -> wxSizer:setSizeHints(Sizer, Parent)
		end;
	false ->
		Sizer = wxWindow:getSizer(Parent),
		wxSizer:add(Sizer, Component, SizerFlags)
		%wxSizer:fit(Sizer, Parent)	%'fit' should not really be called here if possible
	end,
	gx:register(GxName, Component).

%% Opts = [{label, String}] 
button(Parent, Opts) ->
	create_control(Parent, fun() ->
		Label = get_string(label, "OK", Opts),
		wxButton:new(Parent, -1, [{label, Label}])
	end, [], [{onclick, command_button_clicked}], Opts). 

%%
checkbox(Parent, Opts) ->
	create_control(Parent, fun() ->
		Label = get_string(label, "(undefined)", Opts),
		wxCheckBox:new(Parent, -1, Label, [])
	end, [], [{onselect, command_checkbox_clicked}], Opts). 
	
%%
checklist(Parent, Opts) ->
	create_control(Parent, fun() ->
		Choices = get_option(items, [], Opts),
		wxCheckListBox:new(Parent, -1, [{choices, Choices}])
	end, [], [{onselect, command_listbox_selected}], Opts).
	% WXE BUG: command_checklistbox_toggled is not defined

%% 
choice(Parent, Opts) ->
	create_control(Parent, fun() ->
		Choices = get_option(items, [], Opts),
		%BUG - missing define in wx {style, ?wxCB_DROPDOWN}
		wxChoice:new(Parent, -1, [{choices, Choices}])
	end, [], [{onselect, command_choice_selected}], Opts).
	
%% Alias
combo(Parent, Opts) -> combobox(Parent, Opts).
%%
combobox(Parent, Opts) ->
	create_control(Parent, fun() ->
		Value = get_string(value, "", Opts),
		Choices = get_option(items, [], Opts),
		wxComboBox:new(Parent, -1, [{value, Value}, {choices, Choices}])
	end, [], [ 
		{onselect, command_combobox_selected}, 
		{onchange, command_text_updated}
	], Opts). 
	
%% TODO: Doesn't work as expected yet
line(Parent, Opts) ->
	create_control(Parent, fun() ->
		_Label = get_string(label, Opts),
		Orientation = 
			%% Note: Why StaticLine doesn't use ?wxHORIZONTAL and ?wxVERTICAL like every
			%% other component is a total mystery to me
			case get_atom(orientation, horizontal, Opts) of
			vertical -> ?wxLI_VERTICAL;
			_ -> ?wxLI_HORIZONTAL
			end,
		wxStaticLine:new(Parent, [{size, {100,2}}, {style, Orientation}])
	end, [], [], Opts). 

%%
list(Parent, Opts) ->
	create_control(Parent, fun() ->
		Choices = get_option(items, [], Opts),
		wxListBox:new(Parent, -1, [{choices, Choices}])
	end, [], [{onselect, command_listbox_selected}], Opts).
	
%% Alias
radio(Parent, Opts) -> radiobutton(Parent, Opts).

%%
radiobox(Parent, Opts) ->
	create_control(Parent, fun() ->
		Label = get_string(label, "", Opts),
		Choices = get_option(items, [], Opts),
		wxRadioBox:new(Parent, -1, Label, {-1, -1}, {-1, -1}, Choices)
	end, [], [{onselect, command_radiobox_selected}], Opts).

%%
radiobutton(Parent, Opts) ->
	create_control(Parent, fun() ->
		Label = get_string(label, "(undefined)", Opts),
		wxRadioButton:new(Parent, -1, Label, [])
	end, [], [{onselect, command_radiobutton_selected}], Opts). 

%%
slider(Parent, Opts) ->
	create_control(Parent, fun() ->
		Start = get_integer(min, 0, Opts),
		End = get_integer(max, 10, Opts),
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
		wxSlider:new(Parent, -1, Value, Start, End, [{style, Style}])
	end, [], [{onchange, command_slider_updated}], Opts). 

%
spinner(Parent, Opts) ->
	create_control(Parent, fun() ->
		Start = get_integer(min, 0, Opts),
		End = get_integer(max, 10, Opts),
		Value = get_integer(value, 5, Opts),
		Text = get_string(label, "", Opts),
		_Wrap = get_boolean(wrap, false, Opts),
		Style = case get_boolean(wrap, false, Opts) of
			true -> ?wxSP_WRAP;
			false -> 0
		end,
		wxSpinCtrl:new(Parent, [{style, Style}, 
			{min, Start}, {max, End}, {initial, Value}, {value, Text}])
	end, [], [{onchange, command_text_updated}], Opts). 
	% WXE BUG: 'evt_spinctrl' is not defined

%
text(Parent, Opts) ->
	create_control(Parent, fun() ->
		Label = get_string(value, Opts),
		Wrap = get_integer(wrap, -1, Opts),
		StaticText = wxStaticText:new(Parent, -1, Label, []),
		wxStaticText:wrap(StaticText, Wrap),
		StaticText
	end, [], [], Opts). 
	
%%
togglebutton(Parent, Opts) -> 
	create_control(Parent, fun() ->
		Label = get_string(label, "OK", Opts),
		wxToggleButton:new(Parent, -1, Label, [])
	end, [], [{onclick, command_togglebutton_clicked}], Opts). 


%%
%% Complex Controls
%%

%% TODO: Add features! Opts = [{label, String}]
editor(Parent, Opts) ->
	create_control(Parent, fun() ->
		_Value = get_option(value, "Untitled", Opts),
		wxStyledTextCtrl:new(Parent)
	end, [], [], Opts). 

%%
tree(Parent, Opts) ->
	GxName = get_atom(id, Opts),
	Label = get_string(label, Opts),
	Tree = wxTreeCtrl:new(Parent), %, [{style, ?wxTR_HIDE_ROOT}]),
	GxIcons = gx_registry:load_icons(),
	ok = wxTreeCtrl:setImageList(Tree, GxIcons),
	Type = get_atom(type, folder, Opts),
	GxIconMap = lookup(gx_iconmap),
	%Root = 
	case proplists:get_value(Type, GxIconMap) of
	undefined ->
		wxTreeCtrl:addRoot(Tree, Label);
	Index when is_integer(Index) -> 
		wxTreeCtrl:addRoot(Tree, Label, [{image, Index}])
	end,
	%wxTreeCtrl:expand(Tree, Root), % NOTE: WX doesn't like this here!
	SizerFlags = create_sizer_flags([{fill, true}|Opts]),
	update(Parent, Tree, SizerFlags),	
	gx:register(GxName, Tree). 
	
%%	
treeitem(Parent = #wx_ref{type=wxTreeCtrl}, Opts) ->
	%Label = get_string(label, Opts),
	Value = get_string(value, Opts),
	%Icon = get_resource(icon, Opts),
	Root = wxTreeCtrl:getRootItem(Parent),
	Type = get_atom(type, file, Opts),
	GxIconMap = lookup(gx_iconmap),
	case proplists:get_value(Type, GxIconMap) of
	undefined ->
		wxTreeCtrl:appendItem(Parent, Root, Value);
	Index when is_integer(Index) -> 
		wxTreeCtrl:appendItem(Parent, Root, Value, [{image, Index}])
	end.

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
	
	%% TODO: can ignore multiple (but illegal) callbacks here... is this VALID???
	Command = case get_callbacks(Opts) of
	[Callback|_] -> 
		set_command(GxName, [	
			{onselect, command_menu_selected}
		], Callback);
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
	Text = get_option(value, "", Opts),
	wxFrame:setStatusText(Parent, Text, []),
	gx:register(GxName, StatusBar).


%%
%% System Dialogs
%%


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

%% TODO: generalize these remove from registry on close!
textentrydialog(Parent, Message, Opts) ->
	Name = get_atom(id, Opts),
	Caption = get_string(title, "", Opts),
	
	Dialog = wxTextEntryDialog:new(Parent, Message,
		[{style, ?wxOK bor ?wxCANCEL}, {caption, Caption}]),
	
	gx:register(Name, Dialog),
	wxDialog:centreOnParent(Dialog),
    wxDialog:showModal(Dialog),
	Text = wxTextEntryDialog:getValue(Dialog),
    wxDialog:destroy(Dialog),
	Text. 


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

%%
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

