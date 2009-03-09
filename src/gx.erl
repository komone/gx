%%
%%
-module(gx).
-author('steve@simulacity.com').

-include_lib("wx/include/wx.hrl").
-compile(export_all).

start() ->
	%% TODO - For now, just get access to the environment
	%% for resource loading purposes..
	case application:load(?MODULE) of 
	_-> ok
	end,
	wx:new(). % minimally
stop() ->
	wx:destroy().

%%
start(Module, UI) ->
	gx:start(),
	case erlang:system_info(smp_support) of 
	true -> spawn_link(?MODULE, init, [Module, UI]);
	false -> {error, not_smp}
	end.

%%
init(Module, [GxTerm]) when is_tuple(GxTerm) ->
	init2(Module, gx:create(GxTerm));
%
init(Module, File) when is_list(File) ->
	{ok, Resource} = find_resource(File),
	{ok, [GxTerm]} = gxml:load(Resource),
	init2(Module, gx:create(GxTerm)).

%%
init2(Module, Frame) ->
	wxFrame:sendSizeEvent(Frame),
	wxWindow:show(Frame),
	loop(Module, Frame).

%%
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
	Msg ->
		apply(Module, on_message, [Msg]),
		loop(Module, Frame)
    after 1000 ->
		% check on externally updated files?
		loop(Module, Frame)
    end.

%% trunk
create({window, Config, Children}) ->
	G = gx:start(),
	wx:batch(fun() -> 
		Frame = window(G, Config),
		create_tree(Frame, Children),
		Frame end).

create(Parent, Component, Opts) ->
	io:format("~p -> ~p ~p~n", [Parent, Component, Opts]),
	apply(gx, Component, [Parent, Opts]).

destroy(Frame) ->
	io:format("Destroying ~p~n", [Frame]),
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
	io:format("RESOURCE ~p~n", [Candidates]),
	find_file(Candidates).

find_file([H|T]) ->
	case filelib:is_regular(H) of
	true -> {ok, filename:absname(H)};
	false -> find_file(T)
	end;
find_file([]) ->
	{error, enoent}.


%% ets lookup here?
config(Component = {_, _, wxFrame, _}, Opts) ->
	Show = get_option(map, false, Opts),
	wxWindow:show(Component, [{show, Show}]).

%%
read(_Name, _Key) ->
	not_implemented.

%%
%% GX/WX Components
%%

%%
frame(Parent, Opts) ->
	Title = get_option(title, "Untitled", Opts),
	X = get_option(width, 400, Opts),
	Y = get_option(height, 300, Opts),
	Frame = wxFrame:new(Parent, -1, Title, [{size, {X, Y}}]),
	wxFrame:setIcon(Frame, get_icon(Opts)),
	wxFrame:connect(Frame, close_window),
	wxFrame:connect(Frame, command_menu_selected), 
	Frame.

%% review what "window" really means
%% Opts = [{title, String}|{width, Integer}|{height, Integer}|{icon, Path}]
window(Parent, Opts) ->
	Title = get_option(title, "Untitled", Opts),
	X = get_option(width, 400, Opts),
	Y = get_option(height, 300, Opts),
	Frame = wxFrame:new(Parent, -1, Title, [{size, {X, Y}}]),
	wxFrame:setIcon(Frame, get_icon(Opts)),
	wxFrame:connect(Frame, close_window),
	wxFrame:connect(Frame, command_menu_selected), 
	Frame.

menubar(Parent, _Opts) ->
    MenuBar = wxMenuBar:new(),
    wxFrame:setMenuBar(Parent, MenuBar),
	MenuBar.

%% Opts = [{label, String}]
menu(Parent, Opts) ->
	Label = get_option(label, "", Opts),
	Menu = wxMenu:new(),
	wxMenuBar:append(Parent, Menu, Label),
	Menu.
	
%% Opts = [{label, String} | {enable, Bool} | {command, Integer}]
menuitem(Parent = {_, _, wxMenu, _}, Opts) ->
	Label = get_option(label, "", Opts),
	Enabled = get_option(enable, true, Opts),
	Callback = get_option(command, ?wxID_NONE, Opts),
	Item = wxMenu:append(Parent, Callback, Label),
	wxMenuItem:enable(Item, [{enable, Enabled}]),
	Item.
	
separator(Parent = {_, _, wxMenu, _}, _Opts) ->
	wxMenu:appendSeparator(Parent).
	
toolbar(Parent, _Opts) ->
	wxFrame:createToolBar(Parent, []).

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
	Title = get_option(title, "Untitled", Opts),
	Editor = wxStyledTextCtrl:new(Parent),
	wxNotebook:addPage(Parent, Editor, Title),
	Editor.

%% Opts = [{label, String}]
button(Parent, Opts) ->
	Label = get_option(label, "OK", Opts),
	Button = wxButton:new(Parent, -1, [{label, Label}]),
	wxButton:connect(Button, command_button_clicked), 
	Button.

%% Opts = [{text, String}]
statusbar(Parent, Opts) ->
	Child = wxFrame:createStatusBar(Parent,[]),
	Text = get_option(text, "", Opts),
	wxFrame:setStatusText(Parent, Text, []),
	Child.

%%
alert(Parent, Message, Opts) ->
	Caption = get_option(title, "", Opts),
	MD = wxMessageDialog:new(Parent, Message,
		[{style, ?wxOK bor ?wxICON_INFORMATION}, {caption, Caption}]),
    wxDialog:showModal(MD),
    wxDialog:destroy(MD). 

