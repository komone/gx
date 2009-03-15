%%
%%
-module(gx_runner).
-version("alpha").
-author('steve@simulacity.com').

-export([start/1]).
-compile(export_all).

%%
start(File) ->
	gx:start(?MODULE, File). 

%%
%% Callbacks
%%
on_close(_Gx, Event) -> 
	print(Event), exit.
on_exit(_Gx, Event) -> 
	print(Event).

on_about(Gx, _Event) ->
    Text =  "GX Test for Erlang\n"
			"* EXPERIMENTAL*\n"
			"Steve Davis 2009                \n",
	gx:alert(Gx, Text, [{title, "About GX Test"}]).

on_message(Gx, Event) ->
	print(Event),
	case Event of
	{gx,win,_,5000,_} -> 
		String = gx:filedialog(Gx, []),
		io:format("FILE CHOSEN ~p~n", [String]);
	_ -> ok
	end.
	%gx:set(win, status, ["Last event at " ++ timestamp()]).

on_click(_Gx, _Event) -> 
	io:format("[GXRUNNER] Clicked!~n", []), 
	error.

print(Message) ->
	io:format("[GXRUNNER] ~p~n", [Message]).

%%
%% Application Internals
%%

%% creates a valid, printable RFC 3339 (ISO 8601) timestamp
timestamp() ->
	{{Y, M, D}, {H, M1, S}} = calendar:universal_time(),
	L = io_lib:format(
		"~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0B.0Z", 
		[Y, M, D, H, M1, S]),
	lists:flatten(L).
