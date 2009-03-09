%%
-module(ex1).

%% GS Reference: http://www.erlang.org/doc/apps/gs/gs_chapter2.html#2.2

-export([init/0, init/1, on_message/1, on_close/0]).

%% Experimental!!
init() -> 
	UI = 
	[{
		window, [{width, 200}, {height, 100}], 
		[{ 
			button, [{label, "Press Me"}], []
		}]
	}],
	gx:start(?MODULE, UI).

%% or use ex1.xml
init(File) ->
	gx:start(?MODULE, File).

%
% Callbacks (currently required)
% names are currently hard coded in gx...
%
on_message(_Message) ->
	io:format("Hello There~n", []).

on_close() ->
	closed.
