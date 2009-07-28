
-module(gx_test).

-compile(export_all).

start() ->
	_Gx = gx:start(?MODULE, "samples/list.xml").
	
init(Gx, Event) ->
	io:format("gx: ~p, parent: ~p~n", [Gx, get(gx)]),
	io:format("event: ~p~n", [Event]),
	io:format("names: ~p~n", [gx:names()]),	
	io:format("origin: ~p~n", [gx:read(Gx, origin)]),
	ok.
	
clicked(Gx, Event) ->	
	io:format("gx: ~p, event: ~p~n", [Gx, Event]),
	io:format("pos: ~p~n", [gx:read(Gx, pos)]),
	io:format("size: ~p~n", [gx:read(Gx, size)]),
	io:format("icon: ~p, max: ~p, fullscreen: ~p~n", [gx:read(Gx, iconized), 
		gx:read(Gx, maximized), gx:read(Gx, fullscreen)]),
	Dialog = gx:dialog(get(gx), [{title,"Test"}]),
	Text = gx:read(Dialog, title),
	gx:config(Gx, {status, "Created Dialog..." ++ Text}),
	gx:config(Dialog, {show, true}),
	ok.

on_close(_, _) ->
	exit.
	
on_message(_Gx, Event) ->
	io:format("event: ~p~n", [Event]).
