%%
-module(ex1).

%% GS Reference: http://www.erlang.org/doc/apps/gs/gs_chapter2.html#2.2

-export([init/0, init/1, clicked/2]).

%% use ex1.xml
init(File) ->
	gx:start(?MODULE, File).

%% or use gxml:load to generate the following term...
init() -> 
	UI = [{frame,[{id, ex1},{width, 200},{height, 100}],
         [{button,[{id, butt},
                   {label, "Press Me"},
                   {onclick, clicked}],
                  []}]}],
	gx:start(?MODULE, UI).

%
% Callback
% 
clicked(_, _) ->
	io:format("Hello There~n", []).
