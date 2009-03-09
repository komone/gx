-module(gs_ex1).

-copyright('Copyright (c) 1991-97 Ericsson Telecom AB').
-vsn('$Revision: /main/release/2 $ ').

-export([init/0]).

init() -> 
    S = gs:start(),
    %% the parent of a top-level window is the gs server
    Win = gs:create(window,S,[{width,200},{height,100}]),
    Butt = gs:create(button,Win,[{label, {text,"Press Me"}}]),
    gs:config(Win, {map,true}),
    loop(Butt). 

loop(Butt) ->
    receive
        {gs, Butt, click, Data, Args} ->
            io:format("Click ~p ~p~n",[Data, Args]),
            loop(Butt);
        {gs, Win, destroy, Data, Args} ->
            io:format("Destroy ~p ~p ~p~n",[Win, Data, Args])
    end.
	
