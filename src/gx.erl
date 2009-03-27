%%
%% GX Framework
%% Copyright 2009 <steven.charles.davis@gmail.com>. All rights reserved.
%% LICENSE: The correct license type has not yet been determined.
%%
-module(gx).
-version("0.2").
-author('steve@simulacity.com').

-include("../include/gx.hrl").
-include_lib("wx/include/wx.hrl").

%% Original Definiton of wx_ref is in the'hidden' header file wx/src/wxe.hrl
-record(wx_ref, {ref, type, state=[]}).

-compile(export_all).
-export([start/2, create_tree/1, create/3, destroy/2]).
-export([ % Components
	window/2, dialog/2,
	panel/2, box/2,
	menubar/2, menu/2, menuitem/2, separator/2,
	toolbar/2, toolitem/2, button/2, statusbar/2, 
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
gen(XmlFile) ->
	gx_xml:generate(XmlFile).
	
%
info(GxName, Component) ->
	error_logger:info_report([
		{process, self()}, 
		{created, GxName}, 
		{component, Component}, 
		{resource_paths, gx_util:get_resource_paths()}]).

% TODO: here is proof that gx should be a gen_server
registry(Pid) when is_pid(Pid) ->
	case is_process_alive(Pid) of
	true ->
		Pid ! {gx, self(), call, registry, []},
		receive
		Msg -> io:format("~p~n", [Msg])
		after 2000 -> no_response
		end;
	false -> no_process
	end.
	
registry() ->
	get().

%%
%% Core GUI startup and event loop
%%
start() ->
	case erlang:system_info(smp_support) of 
	true -> 
		Root = wx:new(), % minimally		
		%% TODO - For now, just get access to the environment
		%% for resource loading purposes..
		case application:get_application(?MODULE) of
		{ok, ?MODULE} -> ok;
		undefined -> application:load(?MODULE)
		end,
		Root;
	false ->
		{error, no_smp}
	end.

stop() ->
	try
		wx:destroy()
	catch
		_:_ -> {ok, already_stopped}
	end.
	
%% Spawn a GUI process instance
start(Module, GUI) when is_atom(Module), is_list(GUI) ->
	gx:start(),
	spawn_link(?MODULE, init, [Module, GUI]). %% TODO: or just spawn...

%% if we have a term definition, load the UI...
init(Module, [GxTerm|T]) when is_tuple(GxTerm) ->
	case T of 
	[] -> ok;
	%% TODO: Keep the following reminder until more than a single top level component
	%% for the UI can be created during init.
	T -> error_logger:warning_report([{unparsed_components, T}])
	end,
	
	Window = gx:create_tree(GxTerm),	

	% trigger the gx:onload event
	do_init_handler(Module, Window, GxTerm),
	% do main loop
	loop(Module, Window);
% ...or else load the xml file definition
init(Module, File) when is_list(File) ->
	{ok, Resource} = gx_util:find_resource(File),
	{ok, TermList} = gx_xml:load(Resource),
	init(Module, TermList).

%% The main event loop for the GUI process instance
loop(Module, Window) when is_atom(Module) ->
    receive 
	%% DEBUG
	Evt = #wx{} ->
		io:format("wxEvent: ~p~n", [Evt]), 
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
				lookup_command(Evt#wx.id)
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
	{gx, Pid, call, F, A} ->  %% NOTE:Debug Use only
		Pid ! {F, self(), apply(gx, F, A)},
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
	Ident = case GxName of
		undefined -> Evt#wx.id;
		_ -> GxName
		end,
	WxRef = Evt#wx.obj,	
	WxData = 
		case Evt#wx.event of
		Cmd = #wxCommand{} -> [ Cmd#wxCommand.cmdString, 
			Cmd#wxCommand.commandInt, Cmd#wxCommand.extraLong];
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
	%% TODO: Breaks the write once rule GAH!
	Command = put(gx_command_index, get(gx_command_index)),
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

%% NOTE: This will disappear in the gen_server refactor
%% Generic property getter/setter
-record(gx_ref, {pid, id, wx}).

%% WARNING: these include experimental remote code!
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

%% WARNING: these include experimental remote code!
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
create_tree({GxType, Opts, Children}) ->
	case gx_map:is_top_level(GxType) of
	true -> 
		Gx = gx:start(),
		wx:batch(fun() -> 
			Window = ?MODULE:GxType(Gx, Opts),
			create_tree(Window, Children),
			
			%%%% TODO: this isn't working right for Frame furniture (statusbar, menu, etc) 
			% wxTopLevelWindow:fitInside(Window),
			% wxFrame:sendSizeEvent(Window),
			wxTopLevelWindow:layout(Window),
			wxTopLevelWindow:fit(Window),
			
			case get_pos(Opts) of
			center -> wxTopLevelWindow:center(Window);
			[X, Y] -> wxTopLevelWindow:move(Window, X, Y)
			end,
			
			info(get_atom(id, Opts), Window),
			
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
	%% end hacks
	
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
	%% IMPORTANT TODO: The following unbelievable hack was forced onto me either
	%% by wxWidgets or by an alignment bug in wxErlang... not yet sure which!
	try begin
		% relies on the fact that Panels will return their sizer ref
		PS = get(Parent),  
		?wxHORIZONTAL = wxBoxSizer:getOrientation(PS),
		io:format("!HORIZONTAL LAYOUT!~n", []),
		%% BUG: there's a bug here somewhere and it's in WXE I think...
		Alignment = 
			case get_atom(align, left, Opts) of 
			left   -> ?wxALIGN_TOP;
			center -> ?wxALIGN_CENTER_VERTICAL;
			right  -> ?wxALIGN_BOTTOM;
			_      -> ?wxALIGN_TOP
			end,
		wxSizerFlags:align(SizerFlags, Alignment),
		
		%% Fill and Border are also orientation dependent
		Fill = get_atom(fill, false, Opts),
			case Fill of % for now fill="true" means fill="both"
			width -> 
				wxSizerFlags:proportion(SizerFlags, 1);
			height -> 
				wxSizerFlags:expand(SizerFlags);
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
		wxSizerFlags:border(SizerFlags, ?wxALL, Border)		
	end catch
	_:_ ->
		% do normal alignment
		Alignment2 = 
			case get_atom(align, left, Opts) of
			left   -> ?wxALIGN_LEFT;
			center -> ?wxALIGN_CENTER_HORIZONTAL bor ?wxALIGN_CENTER_VERTICAL;
			right  -> ?wxALIGN_RIGHT;
			_      -> ?wxALIGN_LEFT
			end,
		wxSizerFlags:align(SizerFlags, Alignment2),
		
		Fill1 = get_atom(fill, false, Opts),
			case Fill1 of % for now fill="true" means fill="both"
			width -> 
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
		Border1 = get_integer(border, 0, Opts),
		wxSizerFlags:border(SizerFlags, ?wxALL, Border1)
	end,
		
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

%% Allow shorthand tag of 'item' inside menus, toolbars, etc
item(Parent = #wx_ref{type=wxMenu}, Opts) ->	
	menuitem(Parent, Opts);
item(Parent = #wx_ref{type=wxToolBar}, Opts) ->	
	toolitem(Parent, Opts);
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
window(Parent = #wx_ref{type=wx}, Opts) ->
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
	gx:register(GxName, Panel, Sizer).

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
tabs(Parent, Opts) ->
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

%% TODO: Add features! Opts = [{label, String}]
editor(Parent, Opts) ->
	create_control(Parent, fun() ->
		_Value = get_option(value, "Untitled", Opts),
		wxStyledTextCtrl:new(Parent)
	end, [], [], Opts). 


%%
%% Basic Controls
%%

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

% or label?
text(Parent, Opts) ->
	create_control(Parent, fun() ->
		Label = 
			case get_string(value, Opts) of 
			[] -> get_string(label, Opts);
			Value -> Value
			end,
		Wrap = get_integer(wrap, -1, Opts),
		StaticText = wxStaticText:new(Parent, -1, Label, []),
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
	%wxTreeCtrl:expand(Tree, Root), % NOTE: WX doesn't like this here!
	ok = set_options(Parent, Tree, [{fill, true}|Opts]),
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
	Type = get_atom(type, normal, Opts),
	
	%% TODO: can ignore multiple (but illegal) callbacks here... is this VALID???
	Command = case get_callbacks(Opts) of
	[Callback|_] -> 
		set_command(GxName, [{onselect, command_menu_selected}], Callback);
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
	gx:register(GxName, Item, Parent).

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

