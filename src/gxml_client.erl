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
	io:format("CLOSE~n", []).
on_exit() ->
	io:format("EXIT~n", []).
	
on_about(Parent) ->
    Text =  "GX Test for Erlang\n"
			"NOTE: Experimental\n"
			"Steve Davis 2009                \n",
	gx:alert(Parent, Text, [{title, "About GX Test"}]).

on_message(Message) ->
	io:format("~p~n", [Message]).

