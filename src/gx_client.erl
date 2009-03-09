%%
%%
-module(gx_client).
-author('steve@simulacity.com').

-export([start/1]).
-compile(export_all).

%% TODO: while this is one step closer to removing wx enum values,
%% we'd like these to be Atoms not Integers.
-define(wxID_NEW, 5002).
-define(wxID_OPEN, 5000).
-define(wxID_EXIT, 5006).
-define(wxID_HELP_CONTENTS, 5015).
-define(wxID_ABOUT, 5014).

% Internal definition
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

% External definition
start(File) ->
	{ok, UI} = file:consult(File),
	gx:start(?MODULE, UI).

%%
on_close() ->
	io:format("CLOSE~n", []).
	
%%
on_exit() ->
	io:format("EXIT~n", []).
	
%%
on_about(Parent) ->
    Text =  "GX Test for Erlang\n"
			"NOTE: Experimental\n"
			"Steve Davis 2009                \n",
	gx:alert(Parent, Text, [{title, "About GX Test"}]).

%%
on_message(Message) ->
	io:format("[~p] ~p~n", [self(), Message]).

