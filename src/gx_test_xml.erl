%%
%%
-module(gx_test_xml).
-author('steve@simulacity.com').

-include_lib("wx/include/wx.hrl").
-export([start/0]).
%-compile(export_all).

start() ->
	Frame = gx:load("src/gx_test.xml"), 
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
about_dialog(Parent) ->
    Text =  "GX Test for Erlang\n"
			"NOTE: Experimental\n"
			"Steve Davis 2009                \n",
	gx:alert(Parent, Text, [{title, "About GX Test"}]).
