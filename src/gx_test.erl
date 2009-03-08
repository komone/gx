%%
%%
-module(gx_test).
-include_lib("wx/include/wx.hrl").

-export([start/0]).
%-compile(export_all).

start() ->
	UI = {gui, [
	{window, [{title, "TEST"}, {width, 600}, {height, 400}, {icon, "./priv/wxe.xpm"}], 
	[{menubar, [], [
		{menu, 
			[{label, "File"}], 
			[{menuitem, [{label, "&New"}, {command, ?wxID_NEW}]},
			{menuitem, [{label, "&Open"}, {command, ?wxID_OPEN}]},
			{separator, []},
			{menuitem, [{label, "E&xit"}, {command, ?wxID_EXIT}]} 
		]},
		{menu, [{label, "Help"}], [
			{menuitem, [{label, "&Contents"}, {command, ?wxID_HELP_CONTENTS}, {enable, false}]}, 
			{separator, []},
			{menuitem, [{label, "&About...\tF1"}, {command, ?wxID_ABOUT}]}
		]}
	]}
	]},
%	{toolbar, [], [
%		{tool, [{label, "New"}, {icon, "./priv/wxe.xpm"}]}
%	]},
	{tabs, [], [
		{editor, [{title, "Editor"}, {lexer, "Erlang"}], []}
	]},
	{statusbar, [{text, "Ready"}]}
	]},
	
	Frame = gx:create(UI), 
	loop(Frame).

%% enum ids... what to do about this?
loop(Frame) ->
    receive 
  	#wx{event=#wxClose{}} ->
  	    gx:destroy(Frame);
	#wx{id=?wxID_EXIT, event=#wxCommand{type=command_menu_selected}} ->
	    gx:destroy(Frame);
	#wx{id=?wxID_ABOUT, event=#wxCommand{type=command_menu_selected}} ->
	    about_dialog(Frame),
	    loop(Frame);
	Msg ->
		io:format("~p~n", [Msg]),
	    loop(Frame)
    after 1000 ->
		% check on externally updated files
		loop(Frame)
    end.

%%
about_dialog(Frame) ->
    Text =  "GX Test for Erlang\n"
			"NOTE: Experimental\n"
			"Steve Davis 2009                \n",
	gx:alert(Frame, Text, [{title, "About GX Test"}]).
