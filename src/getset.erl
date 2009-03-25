-module(getset).

-compile(export_all).
-export([start/0, start2/0, on_message/2]).

start() ->
	Gx = gx:start(),
	Win = gx:window(Gx, [{id, win}]),
	_Button = gx:button(Win, [{label, "Hello"}]),
	gx:config(win, {title, "Traditional Style"}),
	gx:config(win, [{show, true}, {pos, center}]),
	gx:read(Win, size).


start2() ->
	UI = [{window, [{id, win}, {title, "Term Style"}, 
		{pos, center}, {show, true}], []}],
	gx:start(?MODULE, UI).
	%% !!Here is where we need the gen_server?
	
init(_Gx) ->
	gx:read(win, size).


on_message(Gx, Evt) ->
	io:format("~p ~p~n", [Gx, Evt]).
	
