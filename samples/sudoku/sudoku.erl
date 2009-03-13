%%%-------------------------------------------------------------------
%%% File    : sudoku.erl
%%% Author  : Dan Gudmundsson <dgud@erix.ericsson.se>
%%% Description : Sudoku
%%%
%%% Created : 13 Mar 2007 by Dan Gudmundsson <dgud@erix.ericsson.se>
%%%-------------------------------------------------------------------

-module(sudoku).

-export([go/0]).

-compile(export_all).

-include("sudoku.hrl").

start() -> 
    spawn_link(fun() -> init(halt) end).
go() -> 
    spawn_link(fun() -> init(keep) end).

init(Halt) ->
    GFX = ?TC(start_gfx()),
    case sudoku_game:init(GFX) of
	Halt -> erlang:halt();
	Stop -> exit(Stop)
    end.

start_gfx() ->
    Gfx = spawn_link(sudoku_gui, init_gfx, [self()]),
    receive initiated -> ok
    end,
    Gfx.

tc(Fun,Mod,Line) ->
    case timer:tc(erlang, apply, [Fun,[]]) of
        {_,{'EXIT',Reason}} -> exit(Reason);
        {T,R} ->
            io:format("~p:~p: Time: ~p\n", [Mod, Line, T]),
            R
    end.
