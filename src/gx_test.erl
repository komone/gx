%%
%%
-module(gx_test).
-include_lib("wx/include/wx.hrl").

-export([start/0]).
%-compile(export_all).

start() ->
	UI = {frame, [{title, "TEST"}, {width, 600}, {height, 400}, {icon, "./priv/wxe.xpm"}], 
	[{menubar, [], [
		{menu, 
			[{label, "File"}], 
			[{menuitem, [{label, "&New"}, {callback, ?wxID_NEW}]},
			{menuitem, [{label, "&Open"}, {callback, ?wxID_OPEN}]},
			{separator, []},
			{menuitem, [{label, "E&xit"}, {callback, ?wxID_EXIT}]} 
		]},
		{menu, [{label, "Help"}], [
			{menuitem, [{label, "&Contents"}, {callback, ?wxID_HELP_CONTENTS}, {enable, false}]}, 
			{separator, []},
			{menuitem, [{label, "&About...\tF1"}, {callback, ?wxID_ABOUT}]}
		]}
	]},
%	{toolbar, [], [
%		{tool, [{label, "New"}, {icon, "./priv/wxe.xpm"}]}
%	]},
	{tabs, [], [
		{editor, [{title, "Editor"}], []}
	]},
	{statusbar, [{text, "Ready"}]}
	]},
	
	Frame = gx:create_ui(UI), 
	loop(Frame).

%% enum ids... what to do about this?
loop(Frame) ->
    receive 
  	#wx{event=#wxClose{}} ->
  	    gx:destroy_ui(Frame);
	#wx{id=?wxID_EXIT, event=#wxCommand{type=command_menu_selected}} ->
	    gx:destroy_ui(Frame);
	#wx{id=?wxID_ABOUT, event=#wxCommand{type=command_menu_selected}} ->
	    dialog(?wxID_ABOUT, Frame),
	    loop(Frame);
	Msg ->
		io:format("~p~n", [Msg]),
	    loop(Frame)
    after 1000 ->
		% check on externally updated files
		loop(Frame)
    end.

%%
dialog(?wxID_ABOUT, Frame) ->
    Str = string:join(["GX Test for Erlang\nExperimental Use Only\n", 
		       "Steve Davis 2009\n\n",
		       wx_misc:getOsDescription(), "     \n"], ""),
	MD = wxMessageDialog:new(Frame, Str,
		[{style, ?wxOK bor ?wxICON_INFORMATION}, {caption, "About GX Test"}]),
    wxDialog:showModal(MD),
    wxDialog:destroy(MD).
	
