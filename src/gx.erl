%%
%% GX Framework
%% Copyright (c) 2009 Steve Davis <steven.charles.davis@gmail.com>. All rights reserved.
%% LICENSE: Creative Commons Non-Commercial License V 3.0 
%% http://creativecommons.org/licenses/by-nc/3.0/us/
%%
-module(gx).
-vsn("0.3").
-author('steve@simulacity.com').

-include("../include/gx.hrl").
-include_lib("wx/include/wx.hrl").

-behavior(gen_server).

%% Original Definiton of wx_ref is in the'hidden' header file wx/src/wxe.hrl
-record(wx_ref, {ref, type, state=[]}).

-compile(export_all). % for now...

% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
	terminate/2, code_change/3]).

-export([start/2, create_tree/1, create/3, destroy/2]).
-export([ % Components
	window/2, dialog/2,
	panel/2, box/2,
	menubar/2, menu/2, menuitem/2, separator/2,
	toolbar/2, toolbutton/2, button/2, statusbar/2, 
	tabs/2, editor/2, alert/3, splashscreen/2 % many more...
]). 


%% TODO: remove?
-define(GX_WINDOW_EVENTS, [{onunload, close_window}]).

%
% Convenience functions
%

%
run(File) ->
	gx_runner:start(File).
%
run(Name, File) ->
	gx_runner:start(Name, File).

%
gen(XmlFile) ->
	gx_xml:generate(XmlFile).

%%
%% Core GUI startup
%%
start() ->
	case erlang:system_info(smp_support) of 
	true -> 
		gx_util:set_resource_paths(?MODULE),
		wx:new();
	false ->
		{error, no_smp}
	end.

%% Spawn a GUI server
start(Module, UI) when is_atom(Module), is_list(UI) ->
	start(?MODULE, Module, UI).

%% Spawn a NAMED GUI server (singleton)
start(Name, Module, UI) when is_atom(Module), is_list(UI) ->
	case erlang:system_info(smp_support) of 
	true -> 
		gen_server:start_link({local, Name}, ?MODULE, [Module, UI], []);
	false ->
		{error, no_smp}
	end.
%%
stop() ->
	stop(?MODULE).
stop(Name) ->
    gen_server:call(Name, stop).


%% NOTE: Diverges from GS as there are extended use cases 
%% for example, gx:read(listbox, selected, [{item, 0}]).
% read/2
read(GxName, Property) ->
	read(GxName, Property, []).
% read/3
read(GxName, Property, Opts) when is_atom(GxName), is_tuple(Opts) ->
	read(GxName, Property, [Opts]);
read(GxName, Property, Opts) when is_atom(Property), is_list(Opts) ->
	case whereis(?MODULE) of
	Pid when Pid =:= self() -> 
		gx_map:get(GxName, Property, Opts);
	_ -> 
		gen_server:call(?MODULE, {read, GxName, Property, Opts})
	end.

%% TODO: Extended use case example would be...
%% gx:config(listbox, [{selected, [{item, 0}]}, {style, blah}]).
% config/2
config(GxName, Properties) when is_tuple(Properties) ->
	config(GxName, [Properties]);
config(GxName, Properties) when is_list(Properties) ->
	case whereis(?MODULE) of
	Pid when Pid =:= self() -> 
		[gx_map:set(GxName, Property, Opts) || {Property, Opts} <- Properties];
	_ -> 
		gen_server:call(?MODULE, {config, GxName, Properties})
	end.

%
registry() ->
	registry(?MODULE).
registry(Name) -> 
	case whereis(Name) of
	Pid when Pid =:= self() -> get();
	_ -> gen_server:call(Name, registry)
	end.
%	 
names() -> 
	names(?MODULE).
names(Name) ->
	case whereis(Name) of
	Pid when Pid =:= self() -> 
		SystemNames = [wx_env, 
		gx, gx_paths, gx_icons, gx_iconmap, gx_command_index, 
		'$ancestors', '$initial_call'],
		[X || {X, _} <- get(), is_atom(X), lists:member(X, SystemNames) =:= false];
	_ -> gen_server:call(Name, names)
	end.
	
%%
%% Callbacks for gen_server
%%

%%
init([Module, GUI]) ->
	gx_util:set_resource_paths(Module),
%	io:format("PATHS: ~p~n", []), 
	Window = init_component(Module, GUI),
	{ok, {Module, Window}}.
%%
handle_call(stop, _From, State) -> 
	catch(wx:destroy()),
    {stop, normal, stopped, State};
handle_call({read, Ref = #wx_ref{}, Property, Opts}, _From, State) ->
	Reply = gx_map:get(Ref, Property, Opts),
	{reply, Reply, State};
handle_call({read, GxName, Property, Opts}, _From, State) when is_atom(GxName) ->
	Reply = 
		case lookup(GxName) of
		Ref = #wx_ref{} -> gx_map:get(Ref, Property, Opts);
		_ -> undefined
		end,
	{reply, Reply, State};
handle_call({config, Ref = #wx_ref{}, PropertyList}, _From, State) ->
	Reply = [gx_map:set(Ref, Property, Opts) || {Property, Opts} <- PropertyList],
	{reply, Reply, State};
handle_call({config, GxName, PropertyList}, _From, State) ->
	Reply = 
		case lookup(GxName) of
		Ref = #wx_ref{} -> 
			[gx_map:set(Ref, Property, Opts) || {Property, Opts} <- PropertyList];
		_ -> undefined
		end,
	{reply, Reply, State};
handle_call(registry, _From, State) ->
	{reply, get(), State};
handle_call(names, _From, State) ->
	SystemNames = [wx_env, 
		gx, gx_paths, gx_icons, gx_iconmap, gx_command_index, 
		'$ancestors', '$initial_call'],
	Names = [X || {X, _} <- get(), is_atom(X), lists:member(X, SystemNames) =:= false],
	{reply, Names, State};
handle_call(_Req, _From, State) ->
    {reply, {error, badrequest}, State}.
%%
handle_cast(Req, State) ->
	%% DEBUG - make sure cast messages are not being missed
	io:format("CAST -> ~p~n", [Req]), 
    {noreply, State}.
%% WX Event handling
handle_info(Evt = #wx{}, State) ->
	%io:format("wxEvent: ~p~n", [Evt]), 
	%%% TODO: IN NEED OF REVIEW AND MORE WORK!
	Handler = 
		case Evt#wx.event of 
		#wxClose{} when is_tuple(Evt#wx.userData) ->
			Evt#wx.userData;
		#wxClose{} ->
			{gx, exit};
		#wxCalendar{} ->
			Evt#wx.userData;
		#wxSpin{} ->
			Evt#wx.userData;		
		#wxCommand{} when is_tuple(Evt#wx.userData) ->
			Evt#wx.userData;
		#wxCommand{} -> % menu events
			lookup_command(Evt#wx.id)
		end,
		
	{Module, Window} = State,	
	case validate_handler(Module, Handler) of
	{gx, exit} ->
%		destroy(Window, undefined),
		{stop, normal, State};
	{GxName, GxEvent, Callback} ->
		GxEvt = translate_event(Evt, GxName, GxEvent, Callback),
		case Module:Callback(Window, GxEvt) of 
		exit -> 
%			destroy(Window, GxName),
			{stop, normal, State};
		ok -> 
			{noreply, State};
		Value -> 
			error_logger:error_report([
				{invalid_callback, Callback}, {value, Value}]),
			{noreply, State}
		end;
	undefined ->
		error_logger:error_report([{invalid_handler, Handler}]),
		{noreply, State}
	end;
%%
handle_info(Req, State) ->
	%% DEBUG - make sure non-wx messages to info are not being missed
	io:format("INFO -> ~p~n", [Req]),
    {noreply, State}.
%%
code_change(_Vsn, State, _Extra) ->
    {ok, State}.
%%
terminate(_Reason, State) ->
	try begin
		% IMPL ensure wxWidgets releases the wxImageList (avoid a memory leak)
		case get(gx_icons) of
		Ref = #wx_ref{} -> wxImageList:destroy(Ref);
		_ -> ignore
		end,
		error_logger:info_report([
			{process, self()},
			{destroyed, State}
		]),
		wx:destroy()
	end catch
		_:_ -> {ok, already_stopped}
	end,
    ok.

%%
%% Gen server support functions
%%

%% if we have a term definition, load the UI...
init_component(Module, [GxTerm|T]) when is_tuple(GxTerm) ->
	case T of 
	[] -> ok;
	%% TODO: Keep the following reminder until more than a single top level component
	%% for the UI can be created during init.
	T -> error_logger:warning_report([{unparsed_components, T}])
	end,
	
	Window = gx:create_tree(GxTerm),

	% trigger the gx:onload event
	do_init_handler(Module, Window, GxTerm),
	% return the state!
	Window;
% ...or else load the xml file definition
init_component(Module, File) when is_list(File) ->
	{ok, Resource} = gx_util:find_resource(File),
	{ok, TermList} = gx_xml:load(Resource),
	init_component(Module, TermList).

% Get a valid handler function for an event, if one is available
% It would probably be better to do all this at creation time, if possible
validate_handler(_Module, Handler = {gx, _}) ->
	Handler;
validate_handler(Module, {GxName, GxEvent, GxHandler}) ->
	case erlang:function_exported(Module, GxHandler, 2) of 
	true -> 
		{GxName, GxEvent, GxHandler};
	false -> 
		case erlang:function_exported(Module, on_message, 2) of
		true -> {GxName, GxEvent, on_message};
		false -> undefined
		end
	end.

%% 
translate_event(Evt = #wx{}, GxName, GxEvent, _GxCallback) ->
	Ident = case GxName of
		undefined -> Evt#wx.id;
		_ -> GxName
		end,
	WxRef = Evt#wx.obj,	
	WxData = 
		case Evt#wx.event of
		Cmd = #wxCommand{} -> [ Cmd#wxCommand.cmdString, 
			Cmd#wxCommand.commandInt, Cmd#wxCommand.extraLong ];
		Cmd = #wxSpin{} -> [ [], Cmd#wxSpin.commandInt, 0 ];
		%% BUG: wxCalendarEvent is documented but apparently missing!
		Cmd = #wxCalendar{} -> [wxCalendarCtrl:getDate(WxRef), 0];
		_ -> []
		end,
		
	#gx{id=Ident, 
		type=gx_map:gx_type(WxRef#wx_ref.type), 
		event=GxEvent, 
		data=WxData,
		user=[],
		wx=[Evt#wx.id]}. % wx=Evt}.
				
%
do_init_handler(Module, Parent, {_, Options, _}) ->
	case get_atom(onload, undefined, Options) of
	undefined -> ok;
	Function -> 
		case erlang:function_exported(Module, Function, 2) of 
		true ->
			GxName = get_atom(id, Options),
			Module:Function(Parent, #gx{id=GxName, event=Function, wx=#wx{}});
		false -> {error, no_callback_handler}
		end
	end.
	
%
% Core Utility Functions
% The following functions are to extract all the generic code from the
% component creators defined later.

% IMPORTANT TODO: If the option values from GXML were prefiltered and converted to
% their proper types before returning the GX term, this option system could be 
% simplified enormously, since the types will already be guaranteed to be ok...

%% Simple redirection: used to reduce code complexity inside component functions
get_atom(Key, Opts)              -> gx_util:get_atom(Key, undefined, Opts).
get_atom(Key, Default, Opts)     -> gx_util:get_atom(Key, Default, Opts).
get_boolean(Key, Opts)           -> gx_util:get_boolean(Key, false, Opts).
get_boolean(Key, Default, Opts)  -> gx_util:get_boolean(Key, Default, Opts).
get_integer(Key, Opts)           -> gx_util:get_integer(Key, -1, Opts).
get_integer(Key, Default, Opts)  -> gx_util:get_integer(Key, Default, Opts).
get_string(Key, Opts)            -> gx_util:get_string(Key, "", Opts).
get_string(Key, Default, Opts)   -> gx_util:get_string(Key, Default, Opts).
%% BUG:should be -> get_resource(Type, Key, Opts)
get_resource(Key, Opts)          -> gx_util:get_resource(Key, Opts).

get_option(Key, Opts)            -> gx_util:get_option(Key, undefined, Opts).
get_option(color, Default, Opts) -> gx_map:color(get_atom(color, Default, Opts));
get_option(Key, Default, Opts)   -> gx_util:get_option(Key, Default, Opts).

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

%% Extract any valid candidates for event handlers from the component options
get_callbacks(Opts) ->
	% TODO: Convert copes with string input from gxml...
	% I pretty sure this is not the right place to do this should be in gx_xml
	Convert = fun(Name) ->
		case is_atom(Name) of
		true -> Name;
		false -> list_to_atom(Name)
		end
	end,
	[{X, Convert(Y)} || {X, Y} <- Opts, is_atom(X), lists:member(X, ?GX_EVENTS)].


%% Register a GX component
register(GxName, Component) ->
	case GxName of
	undefined -> ignore;
%	_ -> wxWindow:setName(Component, atom_to_list(GxName))
	_ -> undefined = put(GxName, Component) % enforce "write once"
	end,
	Component.

register(GxName, Component, Parent) ->
	gx:register(GxName, Component),
	undefined = put(Component, Parent), % enforce "write once"
	Component.

%%
lookup(GxName) ->
	get(GxName).
	
%% TODO: currently this destroys the UI. This may not always be so...
%% so the name for this function is somewhat badly chosen
%% as it's not the opposite of create...
destroy(Component, GxName) when is_tuple(Component), is_atom(GxName) ->	
	case get(GxName) of
	Component -> Component = erase(GxName);
	_ -> ignore
	end,
	case get(gx_icons) of
	Ref = #wx_ref{} -> wxImageList:destroy(Ref);
	_ -> ignore
	end,
	error_logger:info_report([
		{process, self()},
		{destroyed, Component}
	]),
	%% TDDO: Assumes the parent class is wxWindow...
	%% TODO: should this be wxWindow:close() ?
	wxWindow:destroy(Component).

%% Create a GX command and the user-defined handler
set_command(GxName, WxEvents, {GxEvent, GxHandler}) ->
	[GxCallback] = [{GxEvent, WxEvent, GxHandler} 
		|| {GxEvent1, WxEvent} <- WxEvents, GxEvent =:= GxEvent1],
	add_command(GxName, GxCallback).

%%
add_command(GxName, GxCallback) ->	
	case get(gx_command_index) of
	undefined -> put(gx_command_index, ?wxID_HIGHEST + 1001);
	_ -> ok
	end,
	%% TODO: Incrementing the command index breaks the write once rule GAH!
	Command = put(gx_command_index, get(gx_command_index) + 1),
	undefined = put({command, Command}, {GxName, GxCallback}),
	% error_logger:info_report([{?GX_COMMANDS, add}, {GxName, GxCallback}]),
	Command.

%% TODO?: could do a consistency check on Type...
lookup_command(Command) ->
	{Name, {Event, _WxType, GxCallback}} = get({command, Command}),
	{Name, Event, GxCallback}.

%% TODO: Need to clarify naming in the code to reflect the exact differences 
%% between Handlers, Callbacks and Events 
%% Register/enable all (valid) user defined event handlers
set_events(GxName, Component, WxEvents, GxHandlers) ->
	GxCallbacks = [
		{GxEvent, WxEvent, GxHandler} || 
		{GxEvent, WxEvent} <- WxEvents, 
		{GxEvent1, GxHandler} <- GxHandlers, 
		GxEvent =:= GxEvent1
	],
	connect_callbacks(Component#wx_ref.type, GxName, Component, GxCallbacks).

%% Finally, wire the callbacks from the WX component to the WXE server
connect_callbacks(WxType, GxName, Component, [{GxEvent, WxEvent, GxHandler}|T]) ->
	WxType:connect(Component, WxEvent, [{userData, {GxName, GxEvent, GxHandler}}]),
	connect_callbacks(WxType, GxName, Component, T);
connect_callbacks(_, _, _, []) -> 
	ok.

%
% The Generic GX create/destroy functions
%

%% create/3
create(Component, Parent, Opts) ->
	gx:Component(Parent, Opts).

%% trunk
create_tree({GxType, Opts, Children}) ->
	case gx_map:is_top_level(GxType) of
	true -> 
		Gx = gx:start(),
		undefined = put(gx, Gx),
		wx:batch(fun() -> 
			Window = ?MODULE:GxType(Gx, Opts),
			create_tree(Window, Children),
			
			%%%% TODO: this isn't working right for Frame furniture (statusbar, menu, etc) 
			% wxTopLevelWindow:fitInside(Window),
			% wxFrame:sendSizeEvent(Window),
			wxTopLevelWindow:layout(Window),
			wxTopLevelWindow:fit(Window),
			
			Size = {get_integer(width, Opts), get_integer(height, Opts)}, 
			case Size of
			{W, H} when W =:= -1, H =:= -1 -> ignore;
			{W, H} -> wxTopLevelWindow:setSize(Window, W, H)
			end,
			
			case get_pos(Opts) of
			center -> wxTopLevelWindow:center(Window);
			[X, Y] -> wxTopLevelWindow:move(Window, X, Y)
			end,
			
			error_logger:info_report([
				{process, self()}, 
				{created, get_atom(id, Opts)}, 
				{component, Window}, 
				{resource_paths, get(gx_paths)}]),
			
			case get_boolean(show, true, Opts) of 
			true -> wxTopLevelWindow:show(Window);
			false -> ignore
			end,
			
			Window 
		end);
	false ->
		{error, {not_toplevel, GxType}}
	end.
	
%% branch 
create_tree(Parent, [{Type, Opts, Children} | Rest]) ->
	P = create(Type, Parent, Opts),
	create_tree(P, Children),
	
	%% And now... two hacks for your enjoyment :(
	
	%% IMPL: Important - layout doesn't work without the following!
	%% This is necessary to ensure component sizes are correctly 
	%% propagated from child->parent
	case gx_map:instanceof(P, wxWindow) of 
	true ->
		case wxWindow:getSizer(P) of
		#wx_ref{ref=0} -> ignore; % ? CORRECT ?
		Sizer -> wxSizer:fit(Sizer, Parent)
		end;
	false ->
		ignore
	end,
	%% TDOD: Get rid of this temporary hack...
	%% can it somehow be safely be put back into tree/2?
	case gx_map:instanceof(P, wxTreeCtrl) of
	true ->
		Root = wxTreeCtrl:getRootItem(P),
		wxTreeCtrl:expand(P, Root);
	false -> 
		ignore 
	end,
	%% end hack
	
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
set_options(Parent, Component, Opts) ->
	SizerFlags = wxSizerFlags:new(),
	
	%% BUG: there's a bug here somewhere and it's in WXE I think...
	Flip = 
		try begin
			ParentSizer = wxWindow:getSizer(Parent),
			BoxSizer = wx:typeCast(ParentSizer, wxBoxSizer),
			case wxBoxSizer:getOrientation(BoxSizer) of
			?wxHORIZONTAL -> true;
			_ -> false
			end
		end catch
			error:_ -> false
		end,
	
	Alignment = 
		case get_atom(align, left, Opts) of 
		left when Flip -> ?wxALIGN_TOP;
		left -> ?wxALIGN_LEFT;
		center -> ?wxALIGN_CENTER;
		right when Flip -> ?wxALIGN_BOTTOM;
		right  -> ?wxALIGN_RIGHT;
		_ when Flip -> ?wxALIGN_TOP;
		_  -> ?wxALIGN_LEFT
		end,
	wxSizerFlags:align(SizerFlags, Alignment),
	
	%% Fill and Border are also orientation dependent
	Fill = get_atom(fill, false, Opts),
		case Fill of % for now fill="true" means fill="both"
		width when Flip -> 
			wxSizerFlags:proportion(SizerFlags, 1);
		width -> 
			wxSizerFlags:expand(SizerFlags);
		height when Flip -> 
			wxSizerFlags:expand(SizerFlags);
		height -> 
			wxSizerFlags:proportion(SizerFlags, 1);
		both ->
			wxSizerFlags:expand(SizerFlags),
			wxSizerFlags:proportion(SizerFlags, 1); 
		true -> 
			wxSizerFlags:expand(SizerFlags),
			wxSizerFlags:proportion(SizerFlags, 1); 
		false -> ignore
		end,
	
	% TODO: support border="10, 10, 0, 0" etc
	Border = get_integer(border, 0, Opts),
	wxSizerFlags:border(SizerFlags, ?wxALL, Border),
		
	case wxWindow:isTopLevel(Parent) of 
	true ->
		case wxWindow:getSizer(Component) of
		#wx_ref{ref=0} -> ok;
		Sizer -> wxSizer:setSizeHints(Sizer, Parent)
		end;
	false ->
		Sizer = wxWindow:getSizer(Parent),
		wxSizer:add(Sizer, Component, SizerFlags)
	end,
	ok.
	
%%%
%%% GX/WX Components
%%%

%%
%% Aliases
%%

%
box(Parent, Opts) -> boxpanel(Parent, Opts).
%
combo(Parent, Opts) -> combobox(Parent, Opts).
%
entry(Parent, Opts) -> textentry(Parent, Opts).
% MAYBE TODO: remove frame entirely?
frame(Parent, Opts) -> window(Parent, Opts).
%
label(Parent, Opts) -> text(Parent, Opts).
%
radio(Parent, Opts) -> radiobutton(Parent, Opts).
%
split(Parent, Opts) -> splitpane(Parent, Opts).
%
tabs(Parent, Opts) -> tabbedpane(Parent, Opts).

%% Allow shorthand tag of 'item' inside menus, toolbars, etc
item(Parent = #wx_ref{type=wxMenu}, Opts) ->	
	menuitem(Parent, Opts);
item(Parent = #wx_ref{type=wxToolBar}, Opts) ->	
	toolbutton(Parent, Opts);
item(Parent = #wx_ref{type=wxNotebook}, Opts) ->	
	tabitem(Parent, Opts);
item(Parent = #wx_ref{type=wxTreeCtrl}, Opts) ->	
	treeitem(Parent, Opts);
item(Parent = #wx_ref{type=wxTreeItemId}, Opts) ->	
	treeitem(Parent, Opts).

%%
%% Top Level Components
%%

%% Opts = [{title, String}|{width, Integer}|{height, Integer}|{icon, Path}] etc
window(Parent = #wx_ref{}, Opts) ->
	GxName = get_atom(id, Opts),
	Title = get_string(title, "Untitled", Opts),
	Frame = wxFrame:new(Parent, -1, Title),
	
	Icon = get_resource(icon, Opts),
	wxFrame:setIcon(Frame, Icon),	

	X = get_integer(width, Opts),
	Y = get_integer(height, Opts), 
% IMPL: make into a subpanel so that we can continue to be able to use the
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
dialog(Parent = #wx_ref{}, Opts) ->
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
%% NOTE: Do you ever really need to create a generic wxWindow unless you 
%% are implementing a new type of wx component? Hence, GX doesn't offer it.
%%

%% skel
create_container(_Parent = #wx_ref{}, _Create, _StyleFlags, _Events, _Opts) ->
	not_implemented.

%% Equivalent to what GS calls a 'frame'
panel(Parent, Opts) ->
	GxName = get_atom(id, Opts),
	
	Panel = wxPanel:new(Parent),
	
	case get_option(color, undefined, Opts) of
		undefined -> proceed;
		Color -> wxPanel:setOwnBackgroundColour(Panel, Color)
	end,
	
	Layout = get_atom(layout, column, Opts),
	Sizer = case Layout of
	vertical -> wxBoxSizer:new(?wxVERTICAL);
	column -> wxBoxSizer:new(?wxVERTICAL);
	horizontal -> wxBoxSizer:new(?wxHORIZONTAL);
	row -> wxBoxSizer:new(?wxHORIZONTAL);
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
	
	X = get_integer(width, Opts),
	Y = get_integer(height, Opts),
	wxSizer:setMinSize(Sizer, {X, Y}),
	wxPanel:setSizer(Panel, Sizer),
	
	ok = set_options(Parent, Panel, Opts),	
	gx:register(GxName, Panel, Parent).

%% MAYBE TODO: move this into panel?
boxpanel(Parent, Opts) ->
	GxName = get_atom(id, Opts),
	Label = get_string(label, Opts),
	X = get_integer(width, -1, Opts),
	Y = get_integer(height, -1, Opts),
	
	Panel = wxPanel:new(Parent),
	Sizer = wxBoxSizer:new(?wxVERTICAL),
	
	%% NOTE: wxStaticBox must be added as a first sibling, and not used as
	%% the parent or wxWidgets will *crash* on exit. Thus we enclose the
	%% box in a panel and add in the StaticBox as the first child.		
	Box = wxStaticBox:new(Panel, -1, Label, [{size, {X, Y}}]),
	BoxSizer = wxStaticBoxSizer:new(Box, ?wxVERTICAL),
	wxSizer:add(BoxSizer, Sizer),
	
	wxPanel:setSizer(Panel, BoxSizer),
	
	ok = set_options(Parent, Panel, Opts),
	gx:register(GxName, Panel).

%%
splitpane(Parent, Opts) ->
	GxName = get_atom(id, Opts),
	
	Splitpane = wxSplitterWindow:new(Parent),
	Panel = wxPanel:new(Splitpane), % "holding" panel
	wxPanel:setOwnBackgroundColour(Panel, gx_map:color(blue)),

	Layout = get_atom(layout, vertical, Opts),
	case Layout of
	vertical -> 
		wxSplitterWindow:splitVertically(Splitpane, Panel, Panel);
	horizontal ->
		wxSplitterWindow:splitHorizontally(Splitpane, Panel, Panel);
	_ -> 
		undefined
	end,
	Gravity = get_integer(gravity, 50, Opts) / 10,
	wxSplitterWindow:setSashGravity(Splitpane, Gravity),
	
	X = get_integer(width, Opts),
	Y = get_integer(height, Opts),
	
	Sizer = wxBoxSizer:new(?wxVERTICAL),
	wxSizer:setMinSize(Sizer, {X, Y}),
	
	wxSplitterWindow:setSizer(Splitpane, Sizer),
	
	ok = set_options(Parent, Splitpane, Opts),	
	gx:register(GxName, Splitpane, Sizer).

%%
tabbedpane(Parent, Opts) ->
	GxName = get_atom(id, Opts),
	
	Notebook = wxNotebook:new(Parent, -1, []),
	
	GxIcons = gx_util:load_icons(),
	ok = wxNotebook:setImageList(Notebook, GxIcons),
	
	Sizer = wxBoxSizer:new(?wxVERTICAL),
	wxNotebook:setSizer(Notebook, Sizer),
	
	ok = set_options(Parent, Notebook, Opts), 
	gx:register(GxName, Notebook).

%%
tabitem(Parent = #wx_ref{type=wxNotebook}, Opts) ->
	GxName = get_atom(id, Opts),
	Label = get_string(label, Opts),
	
	Panel = wxPanel:new(Parent), 
	Sizer = wxBoxSizer:new(?wxVERTICAL),
	wxPanel:setSizer(Panel, Sizer),	
	
	IconType = get_atom(type, file, Opts),
	
	GxIconMap = get(gx_iconmap),
	Icon = case proplists:get_value(IconType, GxIconMap) of
	Index when is_integer(Index) -> [{imageId, Index}];
	undefined -> []
	end,
	
	wxNotebook:addPage(Parent, Panel, Label, Icon),
	
	ok = set_options(Parent, Panel, Opts),
	gx:register(GxName, Panel).

% a grid is not a control.... ??!???!
grid(Parent, Opts) ->
	GxName = get_atom(id, Opts),
	Rows = get_integer(rows, -1, Opts),
	Cols = get_integer(cols, -1, Opts),
	
	Grid = wxGrid:new(Parent, -1),
	
	wxGrid:createGrid(Grid, Rows, Cols),
	
	Sizer = wxBoxSizer:new(?wxVERTICAL),
	
	X = get_integer(width, Opts),
	Y = get_integer(height, Opts),
	
	wxSizer:setMinSize(Sizer, {X, Y}),
	wxGrid:setSizer(Grid, Sizer),
	
	ok = set_options(Parent, Grid, Opts),	
	gx:register(GxName, Grid, Sizer).

%%
%% Basic Controls
%%

%% TODO: Validators for all components

%% Generic control
%% TODO: cope with Style flags 
create_control(Parent = #wx_ref{}, Create, _StyleFlags, Events, Opts) ->
	%% NOTE: At the moment it looks as though a higher order function isn't 
	%% necessary here since Create/0 is always called first...
	%% HOWEVER this will change when StyleFlags are implemented.
	Component = Create(), 
	true = gx_map:instanceof(Component, wxControl),

	GxName = get_atom(id, Opts),	
	wxControl:setName(Component, atom_to_list(GxName)),
	
	X = get_integer(width, -1, Opts),
	Y = get_integer(height, -1, Opts),
	wxControl:setMinSize(Component, {X, Y}),
		
	Callbacks = get_callbacks(Opts),
	set_events(GxName, Component, Events, Callbacks),
	
	ok = set_options(Parent, Component, Opts),
	gx:register(GxName, Component, Parent).

%% 
button(Parent, Opts) ->
	create_control(Parent, fun() ->
		Label = get_string(label, "OK", Opts),
		case proplists:is_defined(icon, Opts) of
		true ->
			Icon = get_resource(icon, Opts),
			wxBitmapButton:new(Parent, -1, Icon, [{label, Label}]);
		false ->
			wxButton:new(Parent, -1, [{label, Label}])
		end
	end, [], [{onclick, command_button_clicked}], Opts). 

%% TODO: 3-state checkboxes not yet supported
checkbox(Parent, Opts) ->
	create_control(Parent, fun() ->
		Justify =
			case get_atom(justify, left, Opts) of
			right -> ?wxALIGN_RIGHT;
			_ -> 0
			end,
		Label = get_string(label, "(undefined)", Opts),
		wxCheckBox:new(Parent, -1, Label, [{style, ?wxCHK_2STATE bor Justify}])
	end, [], [{onselect, command_checkbox_clicked}], Opts). 
	
%%
checklist(Parent, Opts) ->
	create_control(Parent, fun() ->
		Choices = get_option(items, [], Opts),
		wxCheckListBox:new(Parent, -1, [{choices, Choices}])
	end, [], [{onselect, command_checklistbox_toggled}], Opts).

%% 
choice(Parent, Opts) ->
	create_control(Parent, fun() ->
		Choices = get_option(items, [], Opts),
		%BUG - missing define in wx {style, ?wxCB_DROPDOWN}
		wxChoice:new(Parent, -1, [{choices, Choices}])
	end, [], [{onselect, command_choice_selected}], Opts).
	
%%
combobox(Parent, Opts) ->
	create_control(Parent, fun() ->
		Value = get_string(value, "", Opts),
		Choices = get_option(items, [], Opts),
		wxComboBox:new(Parent, -1, [{value, Value}, {choices, Choices}])
	end, [], [ 
		{onchange, command_text_updated},
		{onselect, command_combobox_selected}, 
		{onselect, command_text_enter}
	], Opts). 

%%
image(Parent, Opts) ->
	create_control(Parent, fun() ->
		Image = get_resource(src, Opts),
		wxStaticBitmap:new(Parent, -1, Image, [])
	end, [], [], Opts). 

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
	end, [], [
		{ondblclick, command_listbox_doubleclicked},
		{onselect, command_listbox_selected}
	], Opts).
	
%%
progress(Parent, Opts) ->
	create_control(Parent, fun() ->
		Range = get_integer(range, -1, Opts),
		Value = get_integer(value, 0, Opts),
		
		Gauge = wxGauge:new(Parent, -1, Range),
		
		case Range of 
		X when X < 0 -> wxGauge:pulse(Gauge);
		_ -> wxGauge:setValue(Gauge, Value)
		end,
		Gauge
	end, [], [], Opts).

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
	end, [], [{onchange, command_spinctrl_updated}], Opts). 

% or label?
text(Parent, Opts) ->
	create_control(Parent, fun() ->
		Justify = 
			case get_atom(justify, none, Opts) of
			center -> ?wxALIGN_CENTER;
			left   -> ?wxALIGN_LEFT;
			right  -> ?wxALIGN_RIGHT;
			_      -> ?wxALIGN_LEFT
			end,
		Fixed =
			case get_boolean(fixed, false, Opts) of
			true  -> ?wxST_NO_AUTORESIZE;
			false -> 0
			end,
		Label = 
			case get_string(value, Opts) of 
			[] -> get_string(label, Opts);
			Value -> Value
			end,
		Wrap = get_integer(wrap, -1, Opts),
		StaticText = wxStaticText:new(Parent, -1, Label, [{style, Justify bor Fixed}]),
		wxStaticText:wrap(StaticText, Wrap),
		StaticText
	end, [], [], Opts). 

%
textentry(Parent, Opts) ->
	create_control(Parent, fun() ->
		Text = 
			case get_string(value, Opts) of 
			[] -> get_string(text, Opts);
			Value -> Value
			end,
		Style = 
			case get_boolean(multi, false, Opts) of
			true -> ?wxTE_PROCESS_ENTER bor ?wxTE_PROCESS_TAB bor ?wxTE_MULTILINE;
			false -> ?wxTE_PROCESS_ENTER
			end,
		TextCtrl = wxTextCtrl:new(Parent, -1, [{value, Text}, {style, Style}]),
		
		File = get_string(file, [], Opts),
		case File of 
		[] -> ignore;
		Path -> wxTextCtrl:loadFile(TextCtrl, Path)
		end,
		
		Enabled = get_boolean(enabled, true, Opts),
		wxTextCtrl:setEditable(TextCtrl, Enabled),
		TextCtrl
	end, [], [
		{onchange, command_text_updated},
		{onselect, command_text_enter},
		{onsubmit, command_text_enter} %% ...hmmm
	], Opts). 

%%
togglebutton(Parent, Opts) -> 
	create_control(Parent, fun() ->
		Label = get_string(label, "OK", Opts),
		wxToggleButton:new(Parent, -1, Label, [])
	end, [], [{onclick, command_togglebutton_clicked}], Opts). 

%%
%% Composite Controls
%%

%%
calendar(Parent, Opts) ->
	create_control(Parent, fun() ->
		wxCalendarCtrl:new(Parent, -1, [])
	end, [], [
	 {onselect, calendar_sel_changed}, 
	 {onchange, calendar_day_changed}, 
	 {onchange, calendar_month_changed}, 
	 {onchange, calendar_year_changed}, 
	 {ondblclick, calendar_doubleclicked},
	 {onclick, calendar_weekday_clicked}
	], Opts). 

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
	GxIcons = gx_util:load_icons(),
	ok = wxTreeCtrl:setImageList(Tree, GxIcons),
	Type = get_atom(type, folder, Opts),
	GxIconMap = get(gx_iconmap),
	case proplists:get_value(Type, GxIconMap) of
	undefined ->
		wxTreeCtrl:addRoot(Tree, Label);
	Index when is_integer(Index) -> 
		wxTreeCtrl:addRoot(Tree, Label, [{image, Index}])
	end,
	X = get_integer(width, -1, Opts),
	Y = get_integer(height, -1, Opts),
	wxControl:setMinSize(Tree, {X, Y}),
		
	%wxTreeCtrl:expand(Tree, Root), % NOTE: WX doesn't like this here!
	ok = set_options(Parent, Tree, Opts),
	gx:register(GxName, Tree, Parent). 
	
%%	
treeitem(Parent, Opts) ->
	Tree = get_tree_for_treeitem(Parent),	
	Target = case Parent of
	#wx_ref{type=wxTreeCtrl} -> wxTreeCtrl:getRootItem(Tree);
	#wx_ref{type=wxTreeItemId} -> Parent
	end,
	
	Value = get_string(value, Opts),
	Type = get_atom(type, file, Opts),
	GxIconMap = get(gx_iconmap),
	case proplists:get_value(Type, GxIconMap) of
	undefined ->
		Item = wxTreeCtrl:appendItem(Tree, Target, Value);
	Index when is_integer(Index) -> 
		Item = wxTreeCtrl:appendItem(Tree, Target, Value, [{image, Index}])
	end,
	gx:register(undefined, Item, Parent).
%
get_tree_for_treeitem(Ref = #wx_ref{type=wxTreeItemId}) ->
	ParentRef = get(Ref),
	get_tree_for_treeitem(ParentRef);
get_tree_for_treeitem(Ref = #wx_ref{type=wxTreeCtrl}) ->
	Ref.
	
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
	gx:register(GxName, MenuBar, Parent).

%% Opts = [{label, String}]
menu(Parent, Opts) ->
	GxName = get_atom(id, Opts),
	Label = get_string(label, "", Opts),
	Menu = wxMenu:new(),
	wxMenuBar:append(Parent, Menu, Label),	
	gx:register(GxName, Menu, Parent).

%% Opts = [{label, String} | {enable, Bool} | {command, Integer}]
menuitem(Parent = #wx_ref{type=wxMenu}, Opts) -> 
	GxName = get_atom(id, Opts),
	Label = get_string(label, Opts),
	
	%% TODO: can ignore multiple (but illegal) callbacks here... is this VALID???
	Command = case get_callbacks(Opts) of
	[Callback|_] -> 
		set_command(GxName, [{onselect, command_menu_selected}], Callback);
	[] -> ?wxID_NONE
	end,
	
	Type = case get_atom(type, normal, Opts) of 
	radio     -> ?wxITEM_RADIO;
	checkbox  -> ?wxITEM_CHECK;
	separator -> ?wxITEM_SEPARATOR; %% Not really safe...
	_         -> ?wxITEM_NORMAL
	end,
	
	Item = wxMenuItem:new([{parentMenu, Parent}, {id, Command}, {text, Label}, {kind, Type}]),
	
	case get_resource(menuicon, Opts) of 
	undefined -> ignore;
	Icon -> wxMenuItem:setBitmap(Item, Icon)
	end,
	
	Checked = get_boolean(checked, false, Opts),
	wxMenuItem:check(Item, [{check, Checked}]),
	
	Enabled = get_boolean(enabled, true, Opts),
	wxMenuItem:enable(Item, [{enable, Enabled}]),
	
	wxMenu:append(Parent, Item),
	gx:register(GxName, Item, Parent).

%% also for toolbar later...
separator(Parent = #wx_ref{type=wxMenu}, _Opts) ->
	wxMenu:appendSeparator(Parent);
separator(Parent = #wx_ref{type=wxToolBar}, _Opts) ->
	wxToolBar:addSeparator(Parent).

%%
toolbar(Parent, Opts) ->
	GxName = get_atom(id, Opts),
	ToolBar = wxFrame:createToolBar(Parent, []),
	gx:register(GxName, ToolBar).

%% Opts = [{label, String} | {icon, Path}]
toolbutton(Parent = #wx_ref{type=wxToolBar}, Opts) ->
	GxName = get_atom(id, Opts),
	Label = get_string(label, "", Opts),
	
	Command = case get_callbacks(Opts) of
	[Callback|_] -> 
		set_command(GxName, [{onselect, command_menu_selected}], Callback);
	[] -> ?wxID_NONE
	end,
	
	Icon = get_resource(menuicon, Opts),
	ToolButton = wxToolBar:addTool(Parent, Command, Label, Icon),
%% NOTE: doesn't work - need to understand more about wx realize cpp call
	wxToolBar:realize(Parent),
	gx:register(GxName, ToolButton).
 
%% Opts = [{text, String}]
statusbar(Parent, Opts) ->
	GxName = get_atom(id, Opts),
	StatusBar = wxFrame:createStatusBar(Parent, []), 
%	Sizer = wxBoxSizer:new(?wxVERTICAL),
%	wxWindow:setSizer(StatusBar, Sizer),
%	wxSizer:setSizeHints(Sizer, Parent),
	Text = get_option(value, "", Opts),
	wxFrame:setStatusText(Parent, Text, []),
	gx:register(GxName, StatusBar).


%%
%% Predefined dialog types
%%

%% TODO: generalize these remove from registry on close!
%% Ideal would be to create these lazily but keep the reference around
%% after first use
alert(Parent, Message, Opts) ->
	GxName = get_atom(id, Opts),
	Caption = get_string(title, "", Opts),
	
	Dialog = wxMessageDialog:new(Parent, Message,
		[{style, ?wxOK bor ?wxICON_INFORMATION}, {caption, Caption}]),
	
	gx:register(GxName, Dialog),
	wxDialog:centreOnParent(Dialog),
    wxDialog:showModal(Dialog),
    wxDialog:destroy(Dialog),
	ok. 

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
	
%% Actually this ia a type of wxFrame but it makes more sense here 	
%% wxErlang BUG(?): The required style macros are missing from wxErlang (see gx.erl)
splashscreen(Parent, Opts) ->
	Timeout = get_integer(timeout, 5000, Opts),
	Image = get_resource(image, Opts),
	wxSplashScreen:new(Image, 
		?wxSPLASH_CENTRE_ON_SCREEN bor ?wxSPLASH_TIMEOUT, 
		Timeout, Parent, -1).

