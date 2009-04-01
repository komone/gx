-module(crasherl).
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
    Frame = wxFrame:new(Wx, -1, "Button Fix"),
    wxFrame:connect(Frame, close_window),

	FramePanel = wxPanel:new(Frame),
	Sizer = wxBoxSizer:new(?wxVERTICAL),
	wxPanel:setSizer(FramePanel, Sizer),
	wxSizer:setSizeHints(Sizer, Frame),
	
	Panel = wxPanel:new(FramePanel),
	Button = wxButton:new(Panel, -1, [{label, "Centre Me!"}]),
	
	PanelSizer = wxBoxSizer:new(?wxVERTICAL),
	
	%%%%%%%%%%% THIS CALL CRASHES BEAM AT DESTROY TIME %%%%%%%%%%%%%
	wxPanel:setSizer(Panel, PanelSizer), 
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%	HSizer = wxBoxSizer:new(?wxHORIZONTAL),
	ButtonSizer = wxBoxSizer:new(?wxVERTICAL),
	
	SizerFlags = wxSizerFlags:new(),
	wxSizerFlags:align(SizerFlags, ?wxALIGN_CENTRE),
	
	wxSizer:add(ButtonSizer, Button, SizerFlags), %% no tricks

	wxSizerFlags:expand(SizerFlags), %
%	wxSizerFlags:proportion(SizerFlags, 1),
%	wxSizer:add(VSizer, HSizer, SizerFlags),
	wxSizer:add(PanelSizer, ButtonSizer, SizerFlags),
	wxSizer:add(Sizer, PanelSizer, SizerFlags),
	
	wxFrame:setSize(Frame, 400, 300),
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
