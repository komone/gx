%%
%%
%%
-module(gx).
-version("alpha").
-author('steve@simulacity.com').

-include("../include/gx.hrl").
-include("gx_registry.hrl").
-include_lib("wx/include/wx.hrl").

-compile(export_all).

-export([start/2, create/1, create/3, destroy/2, get/2, set/3]).
-export([ % Components
	frame/2, window/2, % dialog/2,
	panel/2, box/2, grid/2,
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
	Frame = gx:create(GxTerm),
	wxFrame:sendSizeEvent(Frame),
	wxWindow:show(Frame),
	loop(Module, Frame);
% ...or else load the xml file definition
init(Module, File) when is_list(File) ->
	{ok, Resource} = gx_registry:find_resource(File),
	{ok, TermList} = gx_xml:load(Resource),
	init(Module, TermList).

%% The main event loop for the GUI process instance
loop(Module, Frame) when is_atom(Module) ->
    receive 
	%% TODO: what if the window doesn't have an id/name??
	Evt = #wx{event=#wxClose{}, userData={GxName, _}} ->
		Function = get_handler(Module, Evt),
		io:format("wxClose: ~p~n", [Function]),
		Module:Function(Frame, Evt),
  	    destroy(Frame, GxName);
	Evt = #wx{} ->
		%% TODO: this is a hack -- improve it!
		Name = case Evt#wx.userData of 
		{GxName, _GxHandler} -> GxName;
		_ -> undefined
		end,
		Function = get_handler(Module, Evt),
		Msg = #gx{id=Name, type=Function, event=Evt#wx.id, data=[Evt]},
		case Module:Function(Frame, Msg) of 
		ok -> loop(Module, Frame);
		exit -> destroy(Frame, Name);
		Value -> 
			io:format("INVALID CALLBACK ~p RETURNED ~p~n", [Function, Value]),
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
get_handler(Module, #wx{userData={_GxName, GxHandler}}) ->
	Exports = Module:module_info(exports),
	case lists:member({GxHandler, 2}, Exports) of 
	true -> 
		GxHandler;
	false -> 
		case lists:member({on_message, 2}, Exports) of
		true -> on_message;
		false -> {error, no_callback_handler}
		end
	end;
get_handler(Module, E = #wx{id=Command}) when is_integer(Command) ->
	GxHandler = gx_registry:lookup_command(Command),
	get_handler(Module, E#wx{userData={undefined, GxHandler}}).

%
% Core Utility Functions
% The following functions are to extract all the generic code from the
% component creators defined later.
%

%% Simple redirection: used to reduce code complexity inside component functions
get_atom(Key, Opts)             -> gx_registry:get_atom(Key, undefined, Opts).
get_atom(Key, Default, Opts)    -> gx_registry:get_atom(Key, Default, Opts).
get_boolean(Key, Default, Opts) -> gx_registry:get_boolean(Key, Default, Opts).
get_integer(Key, Default, Opts) -> gx_registry:get_integer(Key, Default, Opts).
get_string(Key, Opts)           -> gx_registry:get_string(Key, "", Opts).
get_string(Key, Default, Opts)  -> gx_registry:get_string(Key, Default, Opts).
get_resource(icon, Opts)        -> gx_registry:get_resource(icon, Opts).
get_option(Key, Opts)           -> gx_registry:get_option(Key, undefined, Opts).
get_option(Key, Default, Opts)  -> gx_registry:get_option(Key, Default, Opts).

%% Extract valid candidate callbacks from the component definition
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

%% Register a GX command and the user-defined handler
command(GxName, {_WxType, WxMap}, GxHandler) ->
	[GxCallback] = map_callbacks(WxMap, [GxHandler], []),
	gx_registry:add_command(GxName, GxCallback).
	
%% Register/enable all (valid) user defined event handlers
events(GxName, Component, [{WxType, WxMap}|T], GxHandlers) ->
	GxCallbacks = map_callbacks(WxMap, GxHandlers, []),
	%io:format("[CONNECT ] '~p' [~p] ~p~n", [GxName, WxType, HandlerMap]),
	connect_callbacks(WxType, GxName, Component, GxCallbacks),
	%io:format("~n", []),
	events(GxName, Component, T, GxHandlers);
events(_, Component, [], _) -> 
	Component.

%% Associates the WX event directly with the user-defined handler
map_callbacks([WxMap|T], GxHandlers, Acc) ->
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
	io:format("           '~p' {~p, ~p}~n", [GxName, WxEvent, GxHandler]),
	WxType:connect(Component, WxEvent, [{userData, {GxName, GxHandler}}]),
	connect_callbacks(WxType, GxName, Component, T);
connect_callbacks(_, _, _, []) -> 
	ok.


%
% Generic property getter/setter
%
%% TODO: MAP GX properties to the WX function calls

%% TODO: Property is currently directly(!!!) applied as the function name
get(GxName, Property) ->
	Component = lookup(GxName),
	case Component of 
	{wx_ref, _, Type, _} -> Type:Property(Component);
	_ -> undefined
	end.

%%
set(GxName, Action, Args) ->
	%% TODO: Action is currently directly(!!!) applied as the function name
	Component = lookup(GxName),
	case Component of 
	{wx_ref, _, Type, _} -> apply(Type, Action, [Component|Args]);
	_ -> undefined
	end.
	
%%
lookup(GxName) ->
	gx_registry:lookup_component(GxName).

%
% The Generic GX create/destroy functions
%

%% trunk
create({frame, Config, Children}) ->
	GX = gx_registry:start(),
	wx:batch(fun() -> 
		%io:format("[CREATE  ] ~p ~p ~p~n", [window, GX, Config]),
		Frame = frame(GX, Config),
		create_tree(Frame, Children),
		wxWindow:fit(Frame),
		Frame end).

create(Component, Parent, Opts) ->
	%io:format("[CREATE  ] ~p ~p ~p~n", [Component, Parent, Opts]),
	gx:Component(Parent, Opts).

%% branch
create_tree(Parent, [{Component, Opts, Children} | Rest]) ->
	P = create(Component, Parent, Opts),
	create_tree(P, Children),
	create_tree(Parent, Rest);
%% leaf
create_tree(Parent, [{Component, Opts} | Rest]) ->
	create(Component, Parent, Opts),
	create_tree(Parent, Rest);
create_tree(Parent, []) ->
	Parent.

%%
destroy(Component, GxName) when is_tuple(Component), is_atom(GxName) ->	
	gx_registry:remove_component(Component, GxName),
	%% Assume parent_class is wxWindow...
	wxWindow:destroy(Component).
	
	
get_sizer_flags(Opts) ->
	SizerFlags = wxSizerFlags:new(),	
	Align = get_atom(align, center, Opts),
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
	case Fill of
	true -> wxSizerFlags:expand(SizerFlags);
	false -> ignore
	end,
	SizerFlags.

get_orientation(Opts) ->
	case get_atom(orientation, vertical, Opts) of
	horizontal -> ?wxHORIZONTAL;
	vertical -> ?wxVERTICAL
	end.

%%
%% GX/WX Components
%%

%% Opts = [{title, String}|{width, Integer}|{height, Integer}|{icon, Path}]
frame(Parent = #wx_ref{type=wx}, Opts) ->
	GxName = get_atom(id, Opts),
	Title = get_string(title, "Untitled", Opts),
	X = get_integer(width, 50, Opts),
	Y = get_integer(height, 0, Opts),
	Frame = wxFrame:new(Parent, -1, Title, [{size, {X, Y}}]),
	
	Callbacks = get_callbacks(Opts),
	%% NOTE: Should add inherited event mapping macros too, e.g. CLOSE!!!?
	gx:events(GxName, Frame, [?GX_WINDOW_EVENTS], Callbacks),

% removed for now	
	Sizer = wxBoxSizer:new(?wxVERTICAL),
	wxSizer:setMinSize(Sizer, X, Y),

	SizerFlags = wxSizerFlags:new(),
	wxSizerFlags:expand(SizerFlags),	
	wxFrame:setSizer(Frame, Sizer),
	wxSizer:setSizeHints(Sizer, Frame), %% Top-level windows only

	Icon = get_resource(icon, Opts),
	wxFrame:setIcon(Frame, Icon),
	gx:register(GxName, Frame).

%%
window(Parent, Opts) -> 
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
	Orientation = get_orientation(Opts),
	Panel = wxPanel:new(Parent),
	
	Sizer = wxBoxSizer:new(Orientation),
	wxWindow:setSizer(Panel, Sizer),
	ParentSizer = wxWindow:getSizer(Parent),
	SizerFlags = get_sizer_flags(Opts),
	wxSizer:add(ParentSizer, Panel, SizerFlags),
	wxSizer:fit(Sizer, Parent),
	
	gx:register(GxName, Panel).

%% 
box(Parent, Opts) ->
	GxName = get_atom(id, Opts),
	Label = get_string(label, Opts),
	
	%% NOTE: wxStaticBox must be added as a first sibling, and not used as
	%% the parent or wxWidgets will *crash* on exit. Thus we enclose the
	%% box in a panel and add in the StaticBox as the first child.	
	Panel = wxPanel:new(Parent),
	Sizer = wxBoxSizer:new(?wxVERTICAL),
	Box = wxStaticBox:new(Panel, -1, Label),
	BoxSizer = wxStaticBoxSizer:new(Box, ?wxVERTICAL),
	wxSizer:add(BoxSizer, Sizer),
	wxPanel:setSizer(Panel, BoxSizer),
	
	ParentSizer = wxWindow:getSizer(Parent),
	SizerFlags = get_sizer_flags(Opts),
	wxSizer:add(ParentSizer, Panel, SizerFlags),
	wxSizer:fit(Sizer, Parent),
	
	gx:register(GxName, Panel).

grid(_Parent, _Opts) ->
	not_implemented.

%% allow shorthand of 'item' inside lists, menus and toolbars
item(Parent = #wx_ref{type=wxList}, Opts) ->
	listitem(Parent, Opts);
item(Parent = #wx_ref{type=wxMenu}, Opts) ->	
	menuitem(Parent, Opts);
item(Parent = #wx_ref{type=wxToolBar}, Opts) ->	
	toolitem(Parent, Opts).

%% also for toolbar later...
separator(Parent = #wx_ref{type=wxMenu}, _Opts) ->
	wxMenu:appendSeparator(Parent).


menubar(Parent = #wx_ref{type=wxFrame}, Opts) ->
	GxName = get_atom(id, Opts),
    MenuBar = wxMenuBar:new(),
	wxFrame:setMenuBar(Parent, MenuBar),
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
	[Callback|_] = get_callbacks(Opts),
	%% TODO: can ignore multiple (but illegal) callbacks here... is thsi VALID???
	Command = gx:command(GxName, ?GX_MENU_COMMAND, Callback), 
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

list(_Parent, _Opts) ->
	not_implemented.

listitem(_Parent = #wx_ref{type=wxList}, _Opts) ->
	not_implemented.
	
	
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

%%
tabs(Parent, _Opts) ->
	wxNotebook:new(Parent, -1, []).

%% Opts = [{label, String}]
editor(Parent, Opts) ->
	GxName = get_atom(id, Opts),
	Value = get_option(value, "Untitled", Opts),
	Editor = wxTextCtrl:new(Parent, -1, [{value, Value}]),
	
	SizerFlags = wxSizerFlags:new(),	
	
	% TODO support border="10, 10, 0, 0" etc
	Border = get_integer(border, 0, Opts),
	wxSizerFlags:border(SizerFlags, ?wxALL, Border),
	
	Fill = get_boolean(fill, true, Opts),
	case Fill of
	true -> wxSizerFlags:expand(SizerFlags);
	false -> ignore
	end,

	Sizer = wxWindow:getSizer(Parent),
%	io:format("PARENT: ~p SIZER: ~p~n", [Parent, Sizer]),
	wxSizer:add(Sizer, Editor, SizerFlags),
	wxSizer:fit(Sizer, Parent),

	gx:register(GxName, Editor),
	Editor.

%% Opts = [{label, String}]
button(Parent, Opts) ->
	GxName = get_atom(id, Opts),
	Label = get_string(label, "OK", Opts),
	Button = wxButton:new(Parent, -1, [{label, Label}]),
	
	Sizer = wxWindow:getSizer(Parent),
	wxSizer:add(Sizer, Button, get_sizer_flags(Opts)),
	wxSizer:fit(Sizer, Parent),
	
	Callbacks = get_callbacks(Opts),
	gx:events(GxName, Button, [?GX_BUTTON_EVENTS], Callbacks),
	gx:register(GxName, Button).

%% Opts = [{text, String}]
statusbar(Parent, Opts) ->
	GxName = get_atom(id, Opts),
	StatusBar = wxFrame:createStatusBar(Parent,[]),	
	Text = get_option(text, "", Opts),
	wxFrame:setStatusText(Parent, Text, []),
	gx:register(GxName, StatusBar).
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
filedialog(Parent, Opts) ->
	Name = get_atom(id, Opts),
	FileDialog = wxFileDialog:new(Parent),
	gx:register(Name, FileDialog),
    wxDialog:showModal(FileDialog),
	Filename = wxFileDialog:getFilename(FileDialog),
	wxFileDialog:destroy(FileDialog),
	Filename.

