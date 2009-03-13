%%%-------------------------------------------------------------------
-include_lib("wx/include/wx.hrl").

-record(s,{gfx,gen,games=[],p,m,mr,mc,v}).
-define(TC(Cmd), tc(fun() -> Cmd end, ?MODULE, ?LINE)).

-define(NEW,   121).
-define(EMPTY, 122).
-define(HINT,  123).

-define(OPEN,  130).
-define(SAVE,  131).
-define(RULES, 132).
-define(CLEAR, 135).
-define(SHOW_ERROR, 136).
-define(PRINT, 137).
-define(PRINT_PRE, 138).
-define(PRINT_PAGE_SETUP, 139).

-define(TRIVIAL, 240).
-define(EASY,    235).
-define(NORMAL,  230).
-define(HARD,    225).
-define(HARDEST, 210).

-define(QUIT,  ?wxID_EXIT).   %% Use OS specific version if available
-define(ABOUT, ?wxID_ABOUT).  %% Use OS specific 
