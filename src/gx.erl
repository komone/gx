%%
%%
-module(gx).
-author('steve@simulacity.com').

-include_lib("wx/include/wx.hrl").
-compile(export_all).

-export([start/0, start/2, stop/0, create/1]).
-export([config/3, read/2]).
-export([frame/2, window/2,
	menubar/2, menu/2, menuitem/2, toolbar/2, tool/2, 
	button/2, separator/2,
	statusbar/2, tabs/2, editor/2, alert/3]). % more...

-record(gx, {id, type, event, data}).

%%
start(Module, UI) ->
	gx:start(),
	spawn_link(?MODULE, init, [Module, UI]).

start() ->
	case erlang:system_info(smp_support) of 
	true -> 
		Root = wx:new(), % minimally
		
		%% TODO - For now, just get access to the environment
		%% for resource loading purposes..
		application:load(?MODULE),
		
		case ets:info(gxdb) of 
		undefined ->
			ets:new(gxdb, [set, named_table, public]),
			ets:insert(gxdb, {{self(), root}, Root});
		_ -> ok
		end,
		Root;
	false ->
		{error, no_smp}
	end.
	
stop() ->
	case ets:info(gxdb) of
	undefined -> ok;
	_ -> 
		ets:delete(gxdb)
	end,
	wx:destroy().

%%
init(Module, [GxTerm]) when is_tuple(GxTerm) ->
	Frame = gx:create(GxTerm),
	wxFrame:sendSizeEvent(Frame),
	wxWindow:show(Frame),
	io:format("[GXDB  ] ~p~n", [ets:tab2list(gxdb)]),
	loop(Module, Frame);
%
init(Module, File) when is_list(File) ->
	{ok, Resource} = find_resource(File),
	{ok, Term} = gxml:load(Resource),
	init(Module, Term).

%% TODO: remove hard coded callback names
loop(Module, Frame) when is_atom(Module) ->
    receive 
  	#wx{event=#wxClose{}} ->
  	    gx:destroy(Frame),
		apply(Module, on_close, []);
	#wx{id=?wxID_EXIT, event=#wxCommand{type=command_menu_selected}} ->
	    gx:destroy(Frame),
		apply(Module, on_exit, []);
	#wx{id=?wxID_ABOUT, event=#wxCommand{type=command_menu_selected}} ->
		apply(Module, on_about, [Frame]),
		loop(Module, Frame);
	Evt = #wx{} ->
		Cmd = Evt#wx.event,
		% {gs, IdOrName, EventType, Data, Args}
		Msg = #gx{id=Evt#wx.userData, type=Cmd#wxCommand.type, event=Evt#wx.id, data=[]},
		apply(Module, on_message, [Msg]),
		loop(Module, Frame)
    after 1000 ->
		% check on externally updated files?
		loop(Module, Frame)
    end.

%%
register(undefined, _) ->
	true;
register(Name, Component) when is_atom(Name) ->
	case ets:member(gxdb, {self(), Name}) of 
	true -> {error, id_in_use};
	false -> ets:insert_new(gxdb, {{self(), Name}, Component})
	end.
	
%%
lookup(Name) ->
	Process = self(),
	case ets:lookup(gxdb, {Process, Name}) of 
	[{{Process, Name}, Component}] -> Component;
	_ -> undefined
	end.

%% ets lookup here?
config(Name, Action, Args) ->
	Component = lookup(Name),
	case Component of 
	{_, _, Type, _} -> apply(Type, Action, [Component|Args]);
	_ -> undefined
	end.
	
%%
read(Name, Property) ->
	Component = lookup(Name),
	case Component of 
	{_, _, Type, _} -> apply(Type, Property, [Component]);
	_ -> undefined
	end.

%% trunk
create({window, Config, Children}) ->
	G = gx:start(),
	wx:batch(fun() -> 
		Frame = window(G, Config),
		create_tree(Frame, Children),
		Frame end).

create(Parent, Component, Opts) ->
	io:format("[CREATE] ~p~n         ~p ~p~n", [Parent, Component, Opts]),
	apply(gx, Component, [Parent, Opts]).

destroy(Frame) ->
	Process = self(),
	Delete = fun(X, Acc) ->
		case X of 
		{{Process, _}, _} -> ets:delete_object(gxdb, X), Acc + 1;
		_ -> Acc
		end
	end,
	Deleted = ets:foldr(Delete, 0, gxdb),
	io:format("[DESTRY]~p + ~p Refs~n", [Frame, Deleted]),
	wxWindow:destroy(Frame).
	
%% branch
create_tree(Parent, [{Component, Opts, Children} | Rest]) ->
	P = create(Parent, Component, Opts),
	create_tree(P, Children),
	create_tree(Parent, Rest);
%% leaf
create_tree(Parent, [{Component, Opts} | Rest]) ->
	create(Parent, Component, Opts),
	create_tree(Parent, Rest);
create_tree(Parent, []) ->
	Parent.

%%

get_option(id, _, [{id, Value}|_]) when is_list(Value) ->
	list_to_atom(Value);
get_option(Key, _, [{Key, Value}|_]) ->
	Value;
get_option(Key, Default, [_|T]) ->
	get_option(Key, Default, T);
get_option(_, Default, []) ->
	Default.

%%
get_icon(Opts) -> 
	{ok, Icon} = find_resource(get_option(icon, "wxe.xpm", Opts)),
	Type = icon_type(filename:extension(Icon)),
	wxIcon:new(Icon, [{type, Type}]).
%%
icon_type(".xpm") -> ?wxBITMAP_TYPE_XPM;
icon_type(".png") -> ?wxBITMAP_TYPE_PNG;
icon_type(".bmp") -> ?wxBITMAP_TYPE_BMP;
icon_type(_)      -> ?wxBITMAP_TYPE_INVALID.

%% TODO!!
% ./<mypath>/<myfile>
% <myappdir>/<myrsrcpath>/<mypath>/<myfile>
% <myappdir>/<mypath>/<myfile>
% <gxapp>/<gxrsrcpath>/<myfile>
find_resource(File) ->
	AppPaths = 
	case application:get_application() of
	{ok, App} ->
		LibPath = code:lib_dir(App),
		AppPath = filename:join(LibPath, File),
		case application:get_env(resources) of 
		{ok, Resources} -> 
			[filename:join([LibPath, Resources, File]), AppPath];
		undefined -> 
			[AppPath]
		end;
	undefined -> 
		[]
	end,
	GxPaths = 
	case application:get_env(?MODULE, resources) of
	{ok, GxResources} ->
		[filename:join([code:lib_dir(?MODULE), GxResources, File])];
	undefined -> 
		[]
	end,
	Candidates = lists:append([[filename:absname(File)], AppPaths, GxPaths]),
	io:format("[RSRC  ] ~p~n", [Candidates]),
	find_file(Candidates).

find_file([H|T]) ->
	case filelib:is_regular(H) of
	true -> {ok, filename:absname(H)};
	false -> find_file(T)
	end;
find_file([]) ->
	{error, enoent}.

%% GS Events are:
event_type("click") 		-> click;
event_type("doubleclick") 	-> doubleclick;
event_type("configure") 	-> configure;
event_type("enter") 		-> enter;
event_type("leave") 		-> leave;
event_type("motion") 		-> motion;
event_type("buttonpress") 	-> buttonpress;
event_type("buttonrelease") -> buttonrelease;
event_type("focus") 		-> focus;
event_type("destroy") 		-> destroy;
event_type("keypress") 		-> keypress;
event_type("keyrelease") 	-> keyrelease;
event_type(_)		 		-> undefined.

%%
%% GX/WX Components
%%

%%
frame(Parent, Opts) ->
	Name = get_option(id, undefined, Opts),
	Title = get_option(title, "Untitled", Opts),
	X = get_option(width, 400, Opts),
	Y = get_option(height, 300, Opts),
	Frame = wxFrame:new(Parent, -1, Title, [{size, {X, Y}}]),
	gx:register(Name, Frame),
	wxFrame:setIcon(Frame, get_icon(Opts)),
	wxFrame:connect(Frame, close_window),
	wxFrame:connect(Frame, command_menu_selected),
	Frame.

%% review what "window" really means
%% Opts = [{title, String}|{width, Integer}|{height, Integer}|{icon, Path}]
window(Parent, Opts) ->
	Name = get_option(id, undefined, Opts),
	Title = get_option(title, "Untitled", Opts),
	X = get_option(width, 400, Opts),
	Y = get_option(height, 300, Opts),
	Frame = wxFrame:new(Parent, -1, Title, [{size, {X, Y}}]),
	gx:register(Name, Frame),
	wxFrame:setIcon(Frame, get_icon(Opts)),
	wxFrame:connect(Frame, close_window, [{userData, Name}]),
	wxFrame:connect(Frame, command_menu_selected, [{userData, Name}]), 
	Frame.

menubar(Parent, Opts) ->
	Name = get_option(id, undefined, Opts),
    MenuBar = wxMenuBar:new(),
	gx:register(Name, MenuBar),
	wxFrame:setMenuBar(Parent, MenuBar),
	MenuBar.

%% Opts = [{label, String}]
menu(Parent, Opts) ->
	Name = get_option(id, undefined, Opts),
	Label = get_option(label, "", Opts),
	Menu = wxMenu:new(),
	gx:register(Name, Menu),
	wxMenuBar:append(Parent, Menu, Label),
	Menu.
	
%% Opts = [{label, String} | {enable, Bool} | {command, Integer}]
menuitem(Parent = {_, _, wxMenu, _}, Opts) ->
	Name = get_option(id, undefined, Opts),
	Label = get_option(label, "", Opts),
	Enabled = get_option(enable, true, Opts),
	Callback = get_option(command, ?wxID_NONE, Opts),
	Item = wxMenu:append(Parent, Callback, Label),
	gx:register(Name, Item),
	wxMenuItem:enable(Item, [{enable, Enabled}]),
	Item.
	
separator(Parent = {_, _, wxMenu, _}, _Opts) ->
	wxMenu:appendSeparator(Parent).
	
toolbar(Parent, Opts) ->
	Name = get_option(id, undefined, Opts),
	ToolBar = wxFrame:createToolBar(Parent, []),
	gx:register(Name, ToolBar),
	ToolBar.

%% Opts = [{label, String} | {icon, Path}]
tool(_Parent = {_, _, wxToolBar, _}, Opts) ->
	_Label = get_option(label, "", Opts),
	_Icon = get_icon(Opts).
%% NOTE: doesn't work - need to understand more about wx realize cpp call
%	wxToolBar:addTool(Parent, -1, Label, Icon),
%	wxToolBar:realize(Parent).

%%
tabs(Parent, _Opts) ->
	wxNotebook:new(Parent, -1, []).

%% Opts = [{title, String}]
editor(Parent, Opts) ->
	Name = get_option(id, undefined, Opts),
	Title = get_option(title, "Untitled", Opts),
	Editor = wxStyledTextCtrl:new(Parent),
	gx:register(Name, Editor),
	wxNotebook:addPage(Parent, Editor, Title),
	Editor.

%% Opts = [{label, String}]
button(Parent, Opts) ->
	Name = get_option(id, undefined, Opts),
	Label = get_option(label, "OK", Opts),
	Button = wxButton:new(Parent, -1, [{label, Label}]),
	gx:register(Name, Button),
	wxButton:connect(Button, command_button_clicked), 
	Button.

%% Opts = [{text, String}]
statusbar(Parent, Opts) ->
	Name = get_option(id, undefined, Opts),
	StatusBar = wxFrame:createStatusBar(Parent,[]),
	gx:register(Name, StatusBar),
	Text = get_option(text, "", Opts),
	wxFrame:setStatusText(Parent, Text, []),
	StatusBar.

%%
alert(Parent, Message, Opts) ->
	Name = get_option(id, undefined, Opts),
	Caption = get_option(title, "", Opts),
	MD = wxMessageDialog:new(Parent, Message,
		[{style, ?wxOK bor ?wxICON_INFORMATION}, {caption, Caption}]),
	gx:register(Name, MD),		
    wxDialog:showModal(MD),
    wxDialog:destroy(MD). 

