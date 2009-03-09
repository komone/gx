%%
%%
-module(gx).
-author('steve@simulacity.com').

-include_lib("wx/include/wx.hrl").
-compile(export_all).

-define(RESOURCE_PATH, "priv/rsrc").

start() ->
	wx:new(). % minimally
stop() ->
	wx:destroy().

%%
start(Module, UI) ->
	spawn_link(?MODULE, init, [Module, UI]).

%%
init(Module, [GxTerm]) when is_tuple(GxTerm) ->
	init2(Module, gx:create(GxTerm));
%
init(Module, File) when is_list(File) ->
	{ok, [GxTerm]} = gxml:load(File),
	init2(Module, gx:create(GxTerm)).

%%
init2(Module, Frame) ->
	wxFrame:sendSizeEvent(Frame),
	wxWindow:show(Frame),
	loop(Frame, Module).

%%
loop(Frame, Module) when is_atom(Module) ->
    receive 
  	#wx{event=#wxClose{}} ->
  	    gx:destroy(Frame),
		apply(Module, on_close, []);
	#wx{id=?wxID_EXIT, event=#wxCommand{type=command_menu_selected}} ->
	    gx:destroy(Frame),
		apply(Module, on_exit, []);
	#wx{id=?wxID_ABOUT, event=#wxCommand{type=command_menu_selected}} ->
		apply(Module, on_about, [Frame]),
	    loop(Frame, Module);
	Msg ->
		apply(Module, on_message, [Msg]),
	    loop(Frame, Module)
    after 1000 ->
		% check on externally updated files
	    loop(Frame, Module)
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

get_option(Key, _, [{Key, Value}|_]) ->
	Value;
get_option(Key, Default, [_|T]) ->
	get_option(Key, Default, T);
get_option(_, Default, []) ->
	Default.

%% ets lookup here?
config(Component = {_, _, wxFrame, _}, Opts) ->
	Show = get_option(map, false, Opts),
	wxWindow:show(Component, [{show, Show}]).

%%
read(_Name, _Key) ->
	not_implemented.

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

%%
get_icon(Opts) -> 
	Icon = get_option(icon, "priv/rsrc/wxe.xpm", Opts),
	wxIcon:new(Icon, [{type, icontype(filename:extension(Icon))}]).
%%
icontype(".xpm") -> ?wxBITMAP_TYPE_XPM;
icontype(".bmp") -> ?wxBITMAP_TYPE_BMP;
icontype(".png") -> ?wxBITMAP_TYPE_PNG;
icontype(_)      -> ?wxBITMAP_TYPE_INVALID.
