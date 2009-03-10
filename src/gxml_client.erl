%%
%%
-module(gxml_client).
-author('steve@simulacity.com').

-export([start/1]).
-compile(export_all).


start() -> 
	start("test.xml"). 
start(File) ->
	gx:start(?MODULE, File). 

on_close() ->
	io:format("[CLOSE ]~n", []).
on_exit() ->
	io:format("[EXIT  ]~n", []).
	
on_about(Parent) ->
    Text =  "GX Test for Erlang\n"
			"* EXPERIMENTAL*\n"
			"Steve Davis 2009                \n",
	gx:alert(Parent, Text, [{title, "About GX Test"}]).

on_message(Message) ->
	io:format("[EVENT ] ~p~n", [Message]),
	gx:config(win, setStatusText, ["Last event at " ++ timestamp()]).

%% creates a valid, printable RFC 3339 (ISO 8601) timestamp
timestamp() ->
	{{Y, M, D}, {H, M1, S}} = calendar:universal_time(),
	L = io_lib:format(
		"~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0B.0Z", 
		[Y, M, D, H, M1, S]),
	lists:flatten(L).
