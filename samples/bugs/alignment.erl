-module(alignment).
-include_lib("wx/include/wx.hrl").

-export([start/0]).
-compile(export_all).

start() ->
    Wx = wx:new(),
    Frame = wx:batch(fun() -> create_window(Wx) end),
    wxWindow:show(Frame),
    loop(Frame),
    ok.

create_window(Wx) ->
    Frame = wxFrame:new(Wx, -1, "Button Bug", [{size, {600,400}}]),
    wxFrame:createStatusBar(Frame,[]),
    wxFrame:connect(Frame, close_window),
	Panel = wxPanel:new(Frame),
	SizerFlags = wxSizerFlags:new(),


%%% TEST AREA
%	Sizer = wxBoxSizer:new(?wxVERTICAL),
	Sizer = wxBoxSizer:new(?wxHORIZONTAL),
	%%%%
%	wxSizerFlags:align(SizerFlags, ?wxALIGN_CENTRE),
	wxSizerFlags:align(SizerFlags, ?wxALIGN_CENTRE_HORIZONTAL),
%	wxSizerFlags:align(SizerFlags, ?wxALIGN_CENTRE_VERTICAL),
%%%%

	wxPanel:setSizer(Panel, Sizer),
	Button = wxButton:new(Panel, -1, [{label, "Centre Me!"}]),
	wxSizer:add(Sizer, Button, SizerFlags),
	wxSizer:setSizeHints(Sizer, Frame),
    Frame.

loop(Frame) ->
    receive 
  	#wx{event=#wxClose{}} ->
  	    io:format("~p Closing window ~n",[self()]),
  	    wxFrame:destroy(Frame),
  	    ok;
	#wx{id=?wxID_EXIT, event=#wxCommand{type=command_menu_selected}} ->
	    wxWindow:destroy(Frame),
	    ok;
	Msg ->
	    io:format("Got ~p ~n", [Msg]),
	    loop(Frame)
    after 1000 ->
	io:fwrite("."),
	loop(Frame)
    end.
