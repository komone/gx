-module(alignment).
-include_lib("wx/include/wx.hrl").

-export([start/0]).
-compile(export_all).

start() ->
    Wx = wx:new(),
    Frame = wx:batch(fun() -> create_window(Wx) end),
	wxFrame:setSize(Frame, 400, 300),
    wxWindow:show(Frame),
    loop(Frame),
    ok.

create_window(Wx) ->
    Frame = wxFrame:new(Wx, -1, "Button Fix"),
    wxFrame:connect(Frame, close_window),

	Panel = wxPanel:new(Frame),
	Sizer = wxBoxSizer:new(?wxVERTICAL),
			
	Button = wxButton:new(Panel, -1, [{label, "Centre Me!"}]),
	HSizer = wxBoxSizer:new(?wxHORIZONTAL),
	VSizer = wxBoxSizer:new(?wxVERTICAL),

	SizerFlags = wxSizerFlags:new(),
	wxSizerFlags:align(SizerFlags, ?wxALIGN_CENTER),
	wxSizer:add(HSizer, Button, SizerFlags),
	% Don't use expand!
	wxSizerFlags:proportion(SizerFlags, 1),
	wxSizer:add(HSizer, VSizer, SizerFlags),
	wxSizer:add(Sizer, HSizer, SizerFlags),
	wxPanel:setSizer(Panel, Sizer), 
	wxSizer:setSizeHints(Sizer, Frame),
	
    Frame.

loop(Frame) ->
    receive 
  	#wx{event=#wxClose{}} ->
  	    io:format("~p Closing window ~n",[self()]),
  	    wxFrame:destroy(Frame),
  	    ok;
	Msg ->
		io:format("~p~n", [Msg]),
	    loop(Frame)
    after 1000 ->
		loop(Frame)
    end.
