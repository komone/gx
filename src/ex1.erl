-module(ex1).

-copyright('Copyright (c) 1991-97 Ericsson Telecom AB').
-vsn('$Revision: /main/release/2 $ ').
-include_lib("wx/include/wx.hrl").

-export([init/0]).

init() -> 
    G = gx:start(),
    %% the parent of a top-level window is the gs server
    Win = gx:create(G, window, [{width, 200}, {height, 100}]),
    _Butt = gx:create(Win, button, [{label, "Press Me"}]),
    gx:config(Win, [{map, true}]),
    loop(Win). 

loop(Win) ->
    receive
  	#wx{event=#wxClose{}} ->
  	    gx:destroy(Win);
	#wx{id=?wxID_EXIT, event=#wxCommand{type=command_menu_selected}} ->
	    gx:destroy(Win);
	{gs, _Butt, click, _Data, _Args} ->
		io:format("Hello There~n",[]),
		loop(Win);
	Msg -> 
		io:format("~p~n",[Msg]),
		loop(Win)
    end.
