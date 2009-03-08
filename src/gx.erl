%%
%%
-module(gx).
-author('steve@simulacity.com').

-include_lib("wx/include/wx.hrl").
-compile(export_all).

start() ->
	wx:new(). % minimally
stop() ->
	wx:destroy().

load(File) when is_list(File) ->
	{ok, [GxTerm]} = gxml:load(File),
	create(GxTerm).

%% trunk
create({window, Config, Children}) ->
	G = gx:start(),
	wx:batch(fun() -> 
		Frame = window(G, Config),
		create_tree(Frame, Children),
		wxFrame:sendSizeEvent(Frame),
		wxWindow:show(Frame),
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

read(_Name, _Key) ->
	not_implemented.

frame(Parent, Opts) ->
	Title = get_option(title, "Untitled", Opts),
	X = get_option(width, 400, Opts),
	Y = get_option(height, 300, Opts),
	IconFile = get_option(icon, "rsrc/wxe.xpm", Opts),
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
	IconFile = get_option(icon, "rsrc/wxe.xpm", Opts),
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
	Callback = get_option(command, ?wxID_NONE, Opts),
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

%%
tabs(Parent, _Opts) ->
	wxNotebook:new(Parent, -1, []).

%%
editor(Parent, Opts) ->
	Title = get_option(title, "Untitled", Opts),
	Editor = wxStyledTextCtrl:new(Parent),
	wxNotebook:addPage(Parent, Editor, Title),
	Editor.

%%
button(Parent, Opts) ->
	Label = get_option(label, "OK", Opts),
	Button = wxButton:new(Parent, -1, [{label, Label}]),
	wxButton:connect(Button, command_button_clicked), 
	Button.

%%
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


