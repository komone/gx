%%
%%
-module(gx_client).
-author('steve@simulacity.com').

%% the dependency on this header is now reduced to command IDs
-include_lib("wx/include/wx.hrl").

-export([start/0]).
-compile(export_all).


start() ->
	UI = 
	[{window, [{title, "GX TEST"}, {width, 600}, {height, 400}, {icon, "wxe.xpm"}], 
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
	]},
%	{toolbar, [], [
%		{tool, [{label, "New"}, {icon, "./priv/wxe.xpm"}]}
%	]},
	{tabs, [], [
		{editor, [{title, "Editor"}, {lexer, "Erlang"}], []}
	]},
	{statusbar, [{text, "Ready"}]}
	]}],
	gx:start(?MODULE, UI).

on_close() ->
	io:format("CLOSE~n", []).
on_exit() ->
	io:format("EXIT~n", []).
	
on_about(Parent) ->
    Text =  "GX Test for Erlang\n"
			"NOTE: Experimental\n"
			"Steve Davis 2009                \n",
	gx:alert(Parent, Text, [{title, "About GX Test"}]).

on_message(Message) ->
	io:format("[~p] ~p~n", [self(), Message]).

