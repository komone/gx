%%%-------------------------------------------------------------------
%%% File    : sud_board.erl
%%% Author  :  <dgud@erix.ericsson.se>
%%% Description : Manages the gui board
%%%
%%% Created :  9 Jan 2008 by  <dgud@erix.ericsson.se>
%%%-------------------------------------------------------------------
-module(sudoku_board).

-export([new/1, setup_board/2, clear_board/1, left/1,
	 get_board_data/1,set_board_data/2, 
	 event/2, get_butt/3, set_butt/3, butt_correct/3,
	 draw/3]).

-include("sudoku.hrl").

-record(state, {board=[], pen, fonts=[]}).
-record(sq, {key,val,correct=true,given=false}).
    
new(Parent) ->
    Win = wxWindow:new(Parent, ?wxID_ANY),
    wxWindow:setFocus(Win), %% Get keyboard focus
    wxWindow:setSizeHints(Win, {250,250}),
    Me = self(), 
    Paint = fun(Ev,Obj) -> 
		    %% Windows needs to create a wxPaintDC here
		    DC0 = wxPaintDC:new(Win),
		    wxEvent:skip(Obj),
		    wxPaintDC:destroy(DC0),
		    Me ! Ev
	    end,
    wxWindow:connect(Win, paint,  [{callback, Paint}]),
    wxWindow:connect(Win, size,   [{skip, false}]),
    wxWindow:connect(Win, key_up, [{skip, true}]),
    wxWindow:connect(Win, left_down, [{skip, true}]),
    wxWindow:connect(Win, enter_window, [{skip, true}]),

    %% Init pens and fonts
    Pen = wxPen:new({0,0,0}, [{width, 3}]),
    Fs0  = [{Sz,wxFont:new(Sz, ?wxSWISS, ?wxNORMAL, ?wxNORMAL,[])} ||
	       Sz <- [8,9,10,11,12,13,14,16,18,20,22,24,26,28,30,34,38,42,44,46]],
    TestDC  = wxClientDC:new(Win),
    CW = fun({Sz,Font},Acc) ->
		 case wxFont:ok(Font) of
		     true -> 
			 wxDC:setFont(TestDC, Font),
			 CH = wxDC:getCharHeight(TestDC), 
			 [{CH,Sz,Font} | Acc];
		     false ->
			 Acc
		 end
	 end,
    Fs = lists:foldl(CW, [], Fs0),
    wxClientDC:destroy(TestDC),    
    {Win, #state{board=[], pen=Pen, fonts=Fs}}.

draw(DC, Size, {_, S}) ->
    redraw(DC,Size,S).

event({paint, From, DC}, {Win,State}) ->
    Size = wxWindow:getSize(Win),
    NewState = redraw(DC,Size,State),
    From ! {self(), re_painted},
    {Win,NewState};
event(#wx{obj=Win,event=#wxPaint{}}, {Win,State}) ->
    NewState = redraw(Win,State),
    {Win,NewState};
event(#wx{obj=Win,event=#wxSize{}}, {Win,State}) ->
    NewState = redraw(Win,State),
    {Win,NewState};
event(#wx{obj=Win,event=#wxMouse{type=enter_window}}, {Win,State}) ->
    wxWindow:setFocus(Win), %% Get keyboard focus
    {Win,State};
event(_Ev, What) ->
    What.

set_butt(Indx, Val,Board) when is_integer(Indx) ->
    {R,C,_} = sudoku_game:rcm(Indx),
    set_butt({R,C}, Val, Board);

set_butt(Key,0,{Win,S0=#state{board=B0}}) ->  %% Reset
    B = lists:keydelete(Key,2,B0),
    S = S0#state{board=B},
    redraw(Win, S),
    {Win,S};    
set_butt(Key,Val,{Win,S0=#state{board=B0}}) ->
    case lists:keysearch(Key,2,B0) of
	{value, _} -> 
	    B = lists:keyreplace(Key, 2, B0, #sq{key=Key,val=Val});
	false ->
	    B = [#sq{key=Key, val=Val}|B0]
    end,
    S = S0#state{board=B},
    redraw(Win, S),
    {Win,S}.

butt_correct(Key, Correct, {Win,S0=#state{board=B0}}) ->
    case lists:keysearch(Key,2,B0) of
	{value, Butt} -> 
	    B = lists:keyreplace(Key, 2, B0, Butt#sq{key=Key,correct=Correct});
	false ->
	    B = B0	    
    end,
    S = S0#state{board=B},
    redraw(Win, S),
    {Win,S}.

setup_board(Init, {Win, State}) ->    
    B = [#sq{given=true, correct=true, key=Key, val=Val} || {Key,Val} <- Init],
    S = State#state{board=B},
    redraw(Win, S),
    {Win, S}.

clear_board({Win, State = #state{board=B0}}) ->    
    B = [Butt || Butt = #sq{given=true} <- B0],
    S = State#state{board=B},
    redraw(Win, S),
    Given = [{Key, Val} || #sq{key=Key,val=Val,given=true} <- B],
    {{Win, S}, Given}.

get_board_data({_, #state{board=B0}}) ->    
    B0.
set_board_data(B, {Win, S0}) ->    
    S = S0#state{board=B},
    redraw(Win, S),
    G1 = [{Key, Val} || #sq{key=Key,val=Val,given=true} <- B],
    G2 = [{Key, Val} || #sq{key=Key,val=Val,given=false,correct=true} <- B],
    G3 = [{Key, Val} || #sq{key=Key,val=Val,given=false,correct=false} <- B],
    {{Win, S}, G1 ++ G2 ++ G3}.

left({_, #state{board=B}}) ->
    81 - length([ok || #sq{correct=C} <- B, C /= false]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(BRD,10).
-define(ARC_R, 10).

get_butt(X,Y, {Win,_}) ->
    {W0,H0} = wxWindow:getSize(Win),
    BoxSz = getGeomSz(W0,H0),
%%    io:format("~p ~p ~p ~p~n", [{X,Y}, {W0,H0}, BoxSz, calc_pos(X-?BRD,Y-?BRD, BoxSz)]),
    case calc_pos(X-?BRD,Y-?BRD, BoxSz) of
	Pos = {R,C} when 0 < R, R < 10, 0 < C, C < 10 -> Pos;
	_ -> error
    end.
    	         

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

calc_pos(X,Y, BoxSz) ->
    {1+(Y*3 div BoxSz), 1+(X*3 div BoxSz)}.

redraw(Win,S) ->
    DC0  = wxClientDC:new(Win),
    DC   = wxBufferedDC:new(DC0),
    Size = wxWindow:getSize(Win),
    redraw(DC, Size, S),
    wxBufferedDC:destroy(DC),
    wxClientDC:destroy(DC0),
    ok.

redraw(DC, Size, S) ->    
    wx:batch(fun() -> 
		     wxDC:setBackground(DC, ?wxWHITE_BRUSH),
		     wxDC:clear(DC),
		     BoxSz = draw_board(DC,Size,S),
		     F = sel_font(BoxSz div 3,S#state.fonts),
		     [draw_number(DC,F,BoxSz,Sq) || Sq <- S#state.board]
	     end).

sel_font(_BS,[{_H,_Sz,F}]) ->
    %%   io:format("Font sz ~p height ~p in BS ~p~n",[_Sz,_H, _BS]),
    F;
sel_font(BS,[{H,_Sz,F}|_]) when BS > (H + 6) -> 
    %%   io:format("Font sz ~p height ~p in BS ~p~n",[_Sz,H, BS]),
    F;
sel_font(BS,[_|Fs]) ->
    sel_font(BS,Fs).

draw_number(DC,F,Sz,#sq{key={R,C},val=Num,given=Bold,correct=Correct}) ->
    {X,Y} = get_coords(Sz,R-1,C-1),
    TBox = Sz div 3,
    if Bold -> 
	    wxFont:setWeight(F,?wxBOLD),
	    wxDC:setTextForeground(DC,{0,0,0});
       Correct =:= false ->
	    wxFont:setWeight(F,?wxNORMAL),
	    wxDC:setTextForeground(DC,{255,40,40,255});
       true ->
	    wxFont:setWeight(F,?wxNORMAL),
	    wxDC:setTextForeground(DC,{50,50,100,255})
    end,
    wxDC:setFont(DC,F),
    CH = (TBox - wxDC:getCharHeight(DC)) div 2,
    CW = (TBox - wxDC:getCharWidth(DC)) div 2,
    wxDC:drawText(DC, integer_to_list(Num), {X+CW,Y+CH+1}),
    ok.

get_coords(Sz,R,C) ->
    TBox = Sz div 3,
    R1 = R div 3,
    R2 = R rem 3,
    C1 = C div 3,
    C2 = C rem 3,
    {?BRD + C1*Sz + C2*TBox,
     ?BRD + R1*Sz + R2*TBox}.

draw_board(DC,{W0,H0},#state{pen=Pen}) ->
    BoxSz = getGeomSz(W0,H0),
    BS = ?BRD+3*BoxSz,

    wxPen:setWidth(Pen, 3),
    wxPen:setColour(Pen, {0,0,0}),
    wxDC:setPen(DC,Pen),
    
    wxDC:drawRoundedRectangle(DC, {?BRD,?BRD,3*BoxSz+1,3*BoxSz+1}, 
			      float(?ARC_R)),
    %% Testing DrawLines
    wxDC:drawLines(DC, [{?BRD+BoxSz, ?BRD}, {?BRD+BoxSz, BS}]),
    wxDC:drawLine(DC, {?BRD+BoxSz*2, ?BRD}, {?BRD+BoxSz*2, BS}),
    wxDC:drawLine(DC, {?BRD, ?BRD+BoxSz}, {BS, ?BRD+BoxSz}),
    wxDC:drawLine(DC, {?BRD, ?BRD+BoxSz*2}, {BS, ?BRD+BoxSz*2}),

    %% Draw inside lines
    wxPen:setWidth(Pen, 1),
    wxDC:setPen(DC,Pen),
    TBox = BoxSz div 3,   
    wxDC:drawLine(DC, {?BRD+TBox, ?BRD}, {?BRD+TBox, BS}),
    wxDC:drawLine(DC, {?BRD+TBox*2, ?BRD}, {?BRD+TBox*2, BS}),
    wxDC:drawLine(DC, {?BRD+TBox+BoxSz, ?BRD}, {?BRD+TBox+BoxSz, BS}),
    wxDC:drawLine(DC, {?BRD+TBox*2+BoxSz, ?BRD}, {?BRD+TBox*2+BoxSz, BS}),
    wxDC:drawLine(DC, {?BRD+TBox+BoxSz*2, ?BRD}, {?BRD+TBox+BoxSz*2, BS}),
    wxDC:drawLine(DC, {?BRD+TBox*2+BoxSz*2, ?BRD}, {?BRD+TBox*2+BoxSz*2, BS}),
    %% Vert
    wxDC:drawLine(DC, {?BRD, ?BRD+TBox}, {BS, ?BRD+TBox}),
    wxDC:drawLine(DC, {?BRD, ?BRD+TBox*2}, {BS, ?BRD+TBox*2}),
    wxDC:drawLine(DC, {?BRD, ?BRD+TBox+BoxSz}, {BS, ?BRD+TBox+BoxSz}),
    wxDC:drawLine(DC, {?BRD, ?BRD+TBox*2+BoxSz}, {BS, ?BRD+TBox*2+BoxSz}),
    wxDC:drawLine(DC, {?BRD, ?BRD+TBox+BoxSz*2}, {BS, ?BRD+TBox+BoxSz*2}),
    wxDC:drawLine(DC, {?BRD, ?BRD+TBox*2+BoxSz*2}, {BS, ?BRD+TBox*2+BoxSz*2}),
    BoxSz.

getGeomSz(W,H) ->
    Small = if W < H -> W; true -> H end,
    (Small - 2*?BRD) div 3.
