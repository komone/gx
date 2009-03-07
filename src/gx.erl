%%
%%
%%
-module(gx).

-include_lib("wx/include/wx.hrl").

-compile(export_all).

start() ->
	wx:new(). % minimally
stop() ->
	wx:destroy().

%% trunk
create_ui({frame, Config, Children}) ->
	gx:start(),
	wx:batch(fun() -> 
		Frame = frame(gx:start(), Config),
		create_tree(Frame, Children),
		wxFrame:sendSizeEvent(Frame),
		wxWindow:show(Frame),
		Frame end).

destroy_ui(Frame) ->
	io:format("Destroying ~p~n", [Frame]),
	wxFrame:destroy(Frame).
	
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

create(Parent, Component, Opts) ->
	io:format("~p -> ~p~n", [Parent, Component]),
	apply(gx, Component, [Parent, Opts]).

get_option(Key, _, [{Key, Value}|_]) ->
	Value;
get_option(Key, Default, [_|T]) ->
	get_option(Key, Default, T);
get_option(_, Default, []) ->
	Default.

%% ets lookup here?
config(_Name, _Opts) ->
	not_implemented.

read(_Name, _Key) ->
	not_implemented.

frame(Parent, Opts) ->
	Title = get_option(title, "Untitled", Opts),
	X = get_option(width, 400, Opts),
	Y = get_option(height, 300, Opts),
	IconFile = get_option(icon, "../priv/wxe.xpm", Opts),
	Frame = wxFrame:new(Parent, -1, Title, [{size, {X, Y}}]),
	wxFrame:setIcon(Frame, wxIcon:new(IconFile)),
	wxFrame:connect(Frame, close_window),
	wxFrame:connect(Frame, command_menu_selected), 
	Frame.

%% review what "window" really means
window(Parent, Opts) ->
	Title = get_option(title, "Untitled", Opts),
	X = get_option(width, 400, Opts),
	Y = get_option(height, 300, Opts),
	IconFile = get_option(icon, "../priv/wxe.xpm", Opts),
	Frame = wxFrame:new(Parent, -1, Title, [{size, {X, Y}}]),
	wxFrame:setIcon(Frame, wxIcon:new(IconFile)),
	wxFrame:connect(Frame, close_window),
	wxFrame:connect(Frame, command_menu_selected), 
	Frame.

menubar(Parent, _Opts) ->
    MenuBar = wxMenuBar:new(),
    wxFrame:setMenuBar(Parent, MenuBar),
	MenuBar.

menu(Parent, Opts) ->
	Label = get_option(label, "", Opts),
	Menu = wxMenu:new(),
	wxMenuBar:append(Parent, Menu, Label),
	Menu.
	
menuitem(Parent = {_, _, wxMenu, _}, Opts) ->
	Label = get_option(label, "", Opts),
	Enabled = get_option(enable, true, Opts),
	Callback = get_option(callback, ?wxID_NONE, Opts),
	Item = wxMenu:append(Parent, Callback, Label),
	wxMenuItem:enable(Item, [{enable, Enabled}]),
	Item.
	
separator(Parent = {_, _, wxMenu, _}, _Opts) ->
	wxMenu:appendSeparator(Parent).
	
toolbar(Parent, _Opts) ->
	wxFrame:createToolBar(Parent, []).

tool(_Parent = {_, _, wxToolBar, _}, Opts) ->
	_Label = get_option(label, "", Opts),
	_Icon = wxIcon:new(get_option(icon, "", Opts)).
%	wxToolBar:addTool(Parent, -1, Label, Icon),
%	wxToolBar:realize(Parent).

tabs(Parent, _Opts) ->
	wxNotebook:new(Parent, -1, []).

editor(Parent, Opts) ->
	Title = get_option(title, "Untitled", Opts),
	Editor = wxStyledTextCtrl:new(Parent),
	wxNotebook:addPage(Parent, Editor, Title),
	Editor.

button(_Parent, _Opts) ->
	not_implemented.

statusbar(Parent, Opts) ->
	Child = wxFrame:createStatusBar(Parent,[]),
	Text = get_option(text, "", Opts),
	wxFrame:setStatusText(Parent, Text, []),
	Child.

