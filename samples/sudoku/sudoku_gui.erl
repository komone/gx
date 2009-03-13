%%%-------------------------------------------------------------------
%%% File    : sudoku_gui.erl
%%% Author  :  <dgud@erix.ericsson.se>
%%% Description : The Gui and it's event loop
%%%
%%% Created :  9 Jan 2008 by  <dgud@erix.ericsson.se>
%%%-------------------------------------------------------------------
-module(sudoku_gui).

-export([]).
-compile(export_all).

-include("sudoku.hrl").

-import(sudoku_game, [indx/1]).

%%%%%%%%%%  Graphic engine %%%%%%%%%%%%%%

-record(gs,{ids,show_err=true,level=hard,game,frame,orig=[], print_d, print_psdd}).

init_gfx(Game) ->
    Wx = wx:new(),
    {Frame, Butts} = wx:batch(fun() -> create_window(Wx) end),
    Game ! initiated,
    loop_gfx(init_printer(#gs{ids=Butts,game=Game,frame=Frame})).

create_window(Wx) ->
    Frame = wxFrame:new(Wx, -1, "Sudoku", []),

    wxFrame:createStatusBar(Frame,[]),
    wxFrame:connect(Frame, close_window),

    MenuBar = wxMenuBar:new(),
    File    = wxMenu:new([]),
    Opt     = wxMenu:new([]),
    Help    = wxMenu:new([]),

    wxMenu:append(File, ?NEW,  "&New Game"),
    wxMenu:append(File, ?OPEN, "&Open Game"),
    wxMenu:append(File, ?SAVE, "&Save Game"),
    wxMenu:appendSeparator(File),    
    wxMenu:append(File, ?PRINT, "Print"),
    wxMenu:append(File, ?PRINT_PAGE_SETUP, "Page Setup"),
    wxMenu:append(File, ?PRINT_PRE, "Print Preview"),
    wxMenu:appendSeparator(File),
    wxMenu:append(File, ?QUIT, "&Quit Game"),

    wxMenu:append(Help, ?RULES, "Rules"),
    wxMenu:append(Help, ?ABOUT, "About"), 

    wxMenu:appendRadioItem(Opt, ?TRIVIAL, "Level: Trivial"),
    wxMenu:appendRadioItem(Opt, ?EASY, "Level: Easy"),
    LItem = wxMenu:appendRadioItem(Opt, ?NORMAL, "Level: Normal"),
    wxMenu:appendRadioItem(Opt, ?HARD, "Level: Hard"),
    wxMenu:appendRadioItem(Opt, ?HARDEST, "Level: Hardest"),
    wxMenu:appendSeparator(Opt),
    EItem = wxMenu:appendCheckItem(Opt, ?SHOW_ERROR, "Show errors"),

    wxMenuBar:append(MenuBar, File, "&File"),
    wxMenuBar:append(MenuBar, Opt, "O&ptions"),
    wxMenuBar:append(MenuBar, Help, "&Help"),
    
    wxFrame:setMenuBar(Frame, MenuBar),
    wxFrame:connect(Frame, command_menu_selected),

    MainSz = wxBoxSizer:new(?wxVERTICAL),
    Top    = wxBoxSizer:new(?wxHORIZONTAL),

    Panel = wxPanel:new(Frame), 
    NewGame = wxButton:new(Panel, ?NEW, [{label,"New Game"}]),
    wxButton:connect(NewGame, command_button_clicked),
    Empty = wxButton:new(Panel, ?EMPTY, [{label,"Empty Board "}]),
    wxButton:connect(Empty, command_button_clicked),
    Clean = wxButton:new(Panel, ?CLEAR, [{label,"Clear"}]),
    wxButton:connect(Clean, command_button_clicked),
    Hint  = wxButton:new(Panel, ?HINT, [{label, "Hint"}]),
    wxButton:connect(Hint, command_button_clicked),

    wxSizer:addSpacer(Top,2),
    SF = wxSizerFlags:new(),
    wxSizerFlags:proportion(SF,1),
    wxSizer:add(Top, NewGame, wxSizerFlags:left(SF)), 
    wxSizer:addSpacer(Top,3),
    wxSizer:add(Top, Empty,   wxSizerFlags:center(SF)),
    wxSizer:addSpacer(Top,3),   
    wxSizer:add(Top, Clean,   wxSizerFlags:center(SF)),
    wxSizer:addSpacer(Top,3),   
    wxSizer:add(Top, Hint,    wxSizerFlags:right(SF)),

    wxSizer:addSpacer(MainSz,3),
    wxSizer:add(MainSz, Top, wxSizerFlags:center(wxSizerFlags:proportion(SF,0))),
    wxSizer:addSpacer(MainSz,3),

    BState = {Board,_} = sudoku_board:new(Panel),

    wxSizer:add(MainSz, Board, wxSizerFlags:proportion(wxSizerFlags:expand(SF),1)),
    wxWindow:setSizer(Panel,MainSz),
    wxSizer:fit(MainSz, Frame),
    wxSizer:setSizeHints(MainSz,Frame),
    wxWindow:show(Frame),
    %% Check after append so it's initialized on all platforms
    wxMenuItem:check(LItem),
    wxMenuItem:check(EItem),
    {Frame, BState}.

status(Win, F, A) ->
    Str = lists:flatten(io_lib:format(F, A)),
    wxFrame:setStatusText(Win, Str).

loop_gfx(S = #gs{game=G,ids=Ids,orig=Orig,frame=F}) ->
    receive 
	quit ->
	    wxWindow:close(F),
	    wx_core:quit(), 
	    G ! quit;
	{init, Init} ->
	    Board = sudoku_board:setup_board(Init,Ids),
	    status(F, "Given ~p  Left ~p", [length(Init), sudoku_board:left(Board)]),
	    loop_gfx(S#gs{orig=[indx(Id)||{Id,_}<-Init],ids=Board});
	{correct, ButtI} -> 
	    Board = sudoku_board:butt_correct(ButtI, true, Ids),
	    case sudoku_board:left(Board) of
		0 -> 
		    Str = "Congrats, now try a harder one",
		    MD = wxMessageDialog:new(F,Str,
					     [{style, ?wxOK bor ?wxICON_INFORMATION}, 
					      {caption, "Complete"}]),
		    wxDialog:showModal(MD),
		    wxDialog:destroy(MD),
		    status(F, "Given ~p  Left ~p", [length(Orig), 0]);
		L ->
		    status(F, "Given ~p  Left ~p", [length(Orig), L])
	    end,
	    loop_gfx(S#gs{ids=Board});
	{wrong, ButtI} ->
	    case S#gs.show_err of
		true ->
		    Board = sudoku_board:butt_correct(ButtI, false, Ids),
		    loop_gfx(S#gs{ids=Board});
		false ->
		    loop_gfx(S)
	    end;
	{set_val, ButtI, Val} ->
	    Board =  case lists:member(indx(ButtI), Orig) of
			 false -> set_val(ButtI, Val, Ids, G);
			 true ->  Ids
		     end,
	    loop_gfx(S#gs{ids=Board});
	{working, Done} ->
	    status(F, "Thinking: ~p%", [Done]),
	    loop_gfx(S);
	{busy, Mode} ->
	    case Mode of
		start -> wx_misc:beginBusyCursor();
		stop  -> wx_misc:endBusyCursor()
	    end,
	    loop_gfx(S);
	#wx{event=#wxKey{keyCode=KeyC, x=X,y=Y}} ->
	    Val = if KeyC > 47, KeyC < 58 -> KeyC - $0;
		     KeyC > 325, KeyC < 336 -> KeyC - 326; %% NUM LOCK
		     true -> 0
		  end,
	    Id = sudoku_board:get_butt(X,Y,Ids),
	    %% io:format("Got Key ~p at ~p ~n", [Val, Id]), 
 	    Board = case Id =:= error orelse lists:member(indx(Id), Orig) of
			false -> set_val(Id, Val, Ids,G);
			true ->  Ids
		    end,
	    loop_gfx(S#gs{ids=Board});
	#wx{event=#wxMouse{type=left_down,x=X,y=Y}} -> 
	    Id = sudoku_board:get_butt(X,Y,S#gs.ids),
	    case Id =:= error orelse lists:member(indx(Id), Orig) of
		false -> create_popup_menu(self(),Id,X,Y,S#gs.frame);  
		true ->  ignore
	    end,
	    loop_gfx(S);
	#wx{id=?HINT, event=#wxCommand{type=command_button_clicked}} ->
	    G ! {solve,false},
	    loop_gfx(S);
	#wx{event=#wxClose{}} ->
	    catch wxWindow:'Destroy'(F),
	    G ! quit;
	#wx{id=?QUIT, event=#wxCommand{type=command_menu_selected}} ->
	    wxWindow:close(F,[]),
	    G ! quit;
	%% type=command_button_clicked,
	#wx{id=?NEW, event=#wxCommand{}} ->  
	    G ! {op,?NEW,S#gs.level},
	    Board = sudoku_board:setup_board([],Ids),
	    loop_gfx(S#gs{orig=[],ids=Board});
	#wx{id=?EMPTY, event=#wxCommand{}} ->  
	    G ! {op,?EMPTY},
	    Board = sudoku_board:setup_board([],Ids),
	    loop_gfx(S#gs{orig=[],ids=Board});	
	#wx{id=?CLEAR, event=#wxCommand{}} ->  
	    {Board,Vals} = sudoku_board:clear_board(Ids),
	    G ! {loaded, Vals},
	    loop_gfx(S#gs{ids=Board});
	#wx{id=ID, event=#wxCommand{}} when ID > 125 ->
	    New = dialog(ID, S),
	    loop_gfx(New);
	Msg ->
	    sudoku_board:event(Msg, S#gs.ids),
	    loop_gfx(S)
    end.

dialog(?SHOW_ERROR, S=#gs{show_err=Show}) ->
    S#gs{show_err = not Show};
dialog(ID, S) when ID >= 210, ID =< 240 ->
    Level = sudoku_game:level(ID-200),
    S#gs{level = Level};
dialog(?SAVE, S=#gs{frame=Frame, ids=Board}) ->
    FD = wxFileDialog:new(Frame, [{style, ?wxFD_SAVE bor 
				   ?wxFD_OVERWRITE_PROMPT}]),
    case wxFileDialog:showModal(FD) of
	?wxID_OK ->
	    Path = wxFileDialog:getPath(FD),
	    {ok,Fd} = file:open(Path, [write]),
	    List = sudoku_board:get_board_data(Board),
	    io:format(Fd, "~w.~n", [List]),
	    file:close(Fd);
	_ ->
	    ignore
    end,
    wxDialog:destroy(FD),
    S;
dialog(?OPEN, S=#gs{game=Server, frame=Frame, ids=Board0}) ->
    FD = wxFileDialog:new(Frame,[{style, ?wxFD_OPEN bor ?wxFD_FILE_MUST_EXIST}]),
    case wxDialog:showModal(FD) of
	?wxID_OK ->
	    Path = wxFileDialog:getPath(FD),
	    case file:consult(Path) of
		{ok, [Game]} when is_list(Game) ->
		    {Board,Vals} = sudoku_board:set_board_data(Game, Board0),
		    Server ! {loaded, Vals};		    
		_ ->
		    Board = Board0
	    end;
	_ ->
	    Board = Board0
    end,
    wxFileDialog:destroy(FD),
    S#gs{ids=Board};
dialog(?ABOUT,  S=#gs{frame=Frame}) ->
    Str = "I'm just testing WxWidgets.\n"
	"Testing various features and usages.\n/Dgud",
    MD = wxMessageDialog:new(Frame,Str,
			     [{style, ?wxOK bor ?wxICON_INFORMATION}, 
			      {caption, "About box"}]),
    wxDialog:showModal(MD),
    wxDialog:destroy(MD),
    S;
dialog(?RULES, S) ->
    wx_misc:launchDefaultBrowser("http://www.sudoku.com"),
    S;

dialog(?PRINT_PAGE_SETUP, S = #gs{frame=Frame, print_psdd=PsDD0, print_d=PD0}) ->
    wxPageSetupDialogData:setPrintData(PsDD0, PD0),
    PSD = wxPageSetupDialog:new(Frame, [{data,PsDD0}]),
    wxPageSetupDialog:showModal(PSD),
	
    PSDD1 = wxPageSetupDialog:getPageSetupData(PSD),
    PD1 = wxPageSetupDialogData:getPrintData(PSDD1),
    %% Create new objects using copy constr.
    PD = wxPrintData:new(PD1),
    PsDD = wxPageSetupDialogData:new(PSDD1),
    wxPageSetupDialog:destroy(PSD),
    wxPageSetupDialogData:destroy(PsDD0),
    wxPrintData:destroy(PD0),
    S#gs{print_psdd=PsDD, print_d=PD};
dialog(?PRINT_PRE, S = #gs{frame=Frame, print_d=PD}) ->    
    PDD = wxPrintDialogData:new(PD),
    Printout1 = wxPrintout:new("Print", fun(This,Page) -> printout(This,Page,S) end,
			       [{getPageInfo, fun getPageInfo/1}]),
    Printout2 = wxPrintout:new("Print", fun(This,Page) -> printout(This,Page,S) end,
			       [{getPageInfo, fun getPageInfo/1}]),
    Preview = wxPrintPreview:new(Printout1, [{printoutForPrinting,Printout2},{data,PDD}]), 
    case wxPrintPreview:isOk(Preview) of
	true ->
	    PF = wxPreviewFrame:new(Preview, Frame, [{title, "Print Preview"}]),
	    wxPreviewFrame:centre(PF, [{dir, ?wxBOTH}]),
	    wxPreviewFrame:initialize(PF),
	    wxPreviewFrame:centre(PF),
	    wxPreviewFrame:show(PF);
	false ->
	    io:format("Could not create preview window.\n"
		      "Perhaps your current printer is not set correctly?~n", []),
	    wxPrintPreview:destroy(Preview)
    end,
    S;
dialog(?PRINT, S = #gs{frame=Frame, print_d=PD}) ->    
    PDD = wxPrintDialogData:new(PD),
    Printer = wxPrinter:new([{data,PDD}]),
    Printout = wxPrintout:new("Print", fun(This,Page) -> printout(This,Page,S) end,
			      [{getPageInfo, fun getPageInfo/1}]),

    case wxPrinter:print(Printer, Frame, Printout, [{prompt,true}]) of
	false -> 
	    case wxPrinter:getLastError() of
		?wxPRINTER_ERROR ->
		    io:format("There was a problem printing.\n"
			      "Perhaps your current printer is not set correctly?~n", []);
		_ ->
		    io:format("You canceled printing~n", [])
	    end,
	    wxPrinter:destroy(Printer),
	    S;
	true ->
	    PDD2 = wxPrinter:getPrintDialogData(Printer),
	    PD2  = wxPrintDialogData:getPrintData(PDD2),
	    %% Copy data PD2 will be deleted when Printer is destroyed
	    PD3 = wxPrintData:new(PD2),  
	    wxPrintData:destroy(PD),
	    wxPrinter:destroy(Printer),
	    S#gs{print_d = PD3}
    end;

dialog(Other, S) ->
    io:format("other ~p~n",[Other]),
    S.

init_printer(S) ->
    PD   = wxPrintData:new(),

    %% You could set an initial paper size here
    %%    g_printData->SetPaperId(wxPAPER_LETTER); // for Americans
    %%    g_printData->SetPaperId(wxPAPER_A4);    // for everyone else    

    PSDD = wxPageSetupDialogData:new(PD),
    wxPageSetupDialogData:setMarginTopLeft(PSDD,{15,15}),
    wxPageSetupDialogData:setMarginBottomRight(PSDD,{15,15}),

    S#gs{print_d=PD, print_psdd=PSDD}.

getPageInfo(_This) -> 
    {1,1,1,1}.

printout(This, _Page, #gs{ids=Board, print_psdd=PsDD}) ->
    MX = MY = 500,  
    wxPrintout:fitThisSizeToPageMargins(This, {MX,MY}, PsDD),
    
    _DBG = {_X,_Y,W,H} = wxPrintout:getLogicalPageMarginsRect(This, PsDD),
    wxPrintout:offsetLogicalOrigin(This,(W-MX) div 2, (H-MY) div 2),
%%    io:format("~p ->{~p,~p} ~n", [_DBG, (W-MX) div 2, (H-MY) div 2]),

    DC = wxPrintout:getDC(This),
    sudoku_board:draw(DC, {500,500}, Board),
    true.

set_val(Id, Val, Ids0, G) ->
    Ids = sudoku_board:set_butt(Id,Val,Ids0),
    G ! {validate, Id, Val},
    Ids.

%% popupmenu

create_popup_menu(GFX,Butt,X,Y,Frame) ->
    Port = wx:get_env(),
    spawn_link(fun() -> create_popup_menu1(GFX,Butt,Port,X,Y,Frame) end).

create_popup_menu1(GFX,Butt,Port,X,Y,Frame) ->
    wx:set_env(Port),
    PopupMenu = wxMenu:new(),
    create_popup_menu2(1, PopupMenu),

    wxEvtHandler:connect(PopupMenu, command_menu_selected),
    wxWindow:popupMenu(Frame,PopupMenu,X,Y),
    receive 
	#wx{event=#wxCommand{type=command_menu_selected},id=What} ->
	    GFX ! {set_val,Butt,What}
    end.

create_popup_menu2(N,PP) when N > 9 ->
    wxMenu:append(PP, 0, "Clear");
create_popup_menu2(N,PP) ->
    wxMenu:append(PP, N,integer_to_list(N)),
    create_popup_menu2(N+1,PP).


