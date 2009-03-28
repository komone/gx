%%
%% GX Framework
%% Copyright 2009 <steven.charles.davis@gmail.com>. All rights reserved.
%% LICENSE: The correct license type has not yet been determined.
%%
-module(gx_map).
-vsn("0.3").
-author('steve@simulacity.com').

-include_lib("wx/include/wx.hrl").
% IMPL: wxe.hrl is hidden so redefine...
-record(wx_ref, {ref, type, state=[]}).

-compile(export_all).
-export([get/3, set/3, color/1, color/2, color/3]).

%%% DECOMMISSIONED!
process(WxRef = #wx_ref{type=Module}, FType, {Key, Value}) ->
	case gx_map:FType(Module) of
	undefined -> {no_gx_map, {FType, Module, Key}};
	FMap ->
		[{Function, ArgDef}] = [Definition || {FKey, Definition} <- FMap, FKey =:= Key],
		case ArgDef of
		undefined -> 
			{no_gx_map, {FType, Module, Key}};
		_ ->
			Args = map_args(ArgDef, [{wx_ref, WxRef}, {value, Value}]),
			io:format("CALL ~p:~p(~p)~n", [Module, Function, Args]),
			erlang:apply(Module, Function, Args)
		end
	end.
%%% DECOMMISSIONED!
map_args(ArgDef, Opts) ->
	Pred = fun(Arg, Options) -> 
		[Result] = [Value || {Key, Value} <- Options, Key =:= Arg],
		Result
	end,
	[Pred(Arg, Opts) || Arg <- ArgDef].

%
get(WxRef = #wx_ref{type=WxType}, Property, Options) ->
	gx_map:get(WxType, Property, Options, WxRef). 
set(WxRef = #wx_ref{type=WxType}, Property, Options) ->
	gx_map:set(WxType, Property, Options, WxRef). 


% a first go at getter/setters for properties, rather than "undefined" try returning ancestor...?
%get(wxStaticLine, Key) -> map(Key, wxControl, [{vertical, {isVertical, 1}}, {default, {getDefaultSize, 0}}]);
get(wxFrame, size, [], WxRef) -> wxFrame:getSize(WxRef);
get(wxFrame, title, [], WxRef) -> wxFrame:getTitle(WxRef);
get(wxFrame, Function, Opts, WxRef) -> get(wxTopLevelWindow, Function, Opts, WxRef);
get(wxStaticText, label, [], WxRef) -> wxStaticText:getLabel(WxRef);
get(wxStaticText, Function, Opts, WxRef) -> get(wxControl, Function, Opts, WxRef);
%% TODO: many more
get(WxType, Property, Options, WxRef) ->
	case super(WxType) of
	undefined -> undefined;
	Supertype -> get(Supertype, Property, Options, WxRef)
	end.

% a first go at getter/setters for properties, rather than "undefined" try returning ancestor...?
set(wxFrame, pos, center, WxRef) -> wxFrame:centerOnScreen(WxRef);
set(wxFrame, show, Bool, WxRef) when is_boolean(Bool) -> wxFrame:show(WxRef, [{show, Bool}]);
set(wxFrame, show, [], WxRef) -> wxFrame:show(WxRef);
set(wxFrame, title, String, WxRef) when is_list(String) -> wxFrame:setTitle(WxRef, String);
set(wxFrame, Function, Opts, WxRef) -> set(wxWindow, Function, Opts, WxRef);
set(wxStaticText, label, [], WxRef) -> wxStaticText:getLabel(WxRef);
set(wxStaticText, Function, Opts, WxRef) -> set(wxControl, Function, Opts, WxRef);
%% TODO: many more
set(WxType, Property, Options, WxRef) ->
	case super(WxType) of
	undefined -> undefined;
	Supertype -> set(Supertype, Property, Options, WxRef)
	end.


%% TODO: These need double checking!!
%% Map GX to WX types
wx_type(frame) -> wxFrame;
wx_type(window) -> wxFrame;
wx_type(dialog) -> wxDialog;
wx_type(panel) -> wxPanel;
wx_type(checkbox) -> wxCheckBox;
wx_type(checklist) -> wxCheckListBox;
wx_type(splashscreen) -> wxSplashScreen;
wx_type(radiobox) -> wxRadioBox;
wx_type(radiobutton) -> wxRadioButton;
wx_type(choice) -> wxChoice;
wx_type(list) -> wxListBox;
wx_type(image) -> wxStaticBitmap;
wx_type(combo) -> wxComboBox;
wx_type(box) -> wxPanel; % with a BoxSizer maybe panel box="true"?
wx_type(slider) -> wxSlider;
wx_type(spinner) -> wxSpinCtrl;
wx_type(text) -> wxStaticText;
wx_type(line) -> wxStaticLine;
wx_type(entry) -> wxTextCtrl;
wx_type(editor) -> wxStyledTextCtrl;
wx_type(_) -> undefined.

gx_type(wxButton) -> button;
gx_type(wxFrame) -> window;
gx_type(wxDialog) -> dialog;
gx_type(wxPanel) -> panel;
gx_type(wxCheckBox) -> checkbox;
gx_type(wxCheckListBox) -> checklist;
gx_type(wxSplashScreen) -> splashscreen;
gx_type(wxRadioBox) -> radiobox;
gx_type(wxRadioButton) -> radiobutton;
gx_type(wxChoice) -> wxChoice;
gx_type(wxListBox) -> list;
gx_type(wxStaticBitmap) -> image;
gx_type(wxComboBox) -> combo;
gx_type(wxSlider) -> slider;
gx_type(wxSpinCtrl) -> spinner;
gx_type(wxStaticText) -> text;
gx_type(wxStaticLine) -> line;
gx_type(wxTextCtrl) -> entry;
gx_type(wxStyledTextCtrl) -> editor;
gx_type(_) -> undefined.


%%
instanceof(#wx_ref{type=Type}, Class) ->
	instanceof(Type, Class);
instanceof(Type, Class) when is_atom(Type) -> 
	case Type =:= Class of
	true -> true;
	false when Type =:= undefined -> false;
	false -> instanceof(super(Type), Class)
	end.
	
%%
is_top_level(GxType) ->
	WxType = wx_type(GxType),
	instanceof(WxType, wxTopLevelWindow).


%% More concise alternative to wxXXX:parent().
%% Note that any object that is subclassed from
%% wxObject will not appear on this list.
%% Complete as of R13A
super(wxAuiManager)          -> wxEvtHandler;
super(wxAuiNotebook)         -> wxControl;
super(wxBitmapButton)        -> wxButton;
super(wxBitmapDataObject)    -> wxDataObject;
super(wxBoxSizer)            -> wxSizer;
super(wxBufferedDC)          -> wxMemoryDC;
super(wxBufferedPaintDC)     -> wxBufferedDC;
super(wxButton)              -> wxControl;
super(wxCalendarCtrl)        -> wxControl;
super(wxCalendarEvent)       -> wxDateEvent;
super(wxCheckBox)            -> wxControl;
super(wxCheckListBox)        -> wxListBox;
super(wxChildFocusEvent)     -> wxCommandEvent;
super(wxChoice)              -> wxControlWithItems;
super(wxClientDC)            -> wxWindowDC;
super(wxCloseEvent)          -> wxEvent;
super(wxColourDialog)        -> wxDialog;
super(wxColourPickerCtrl)    -> wxPickerBase;
super(wxColorPickerEvent)    -> wxCommandEvent;
super(wxComboBox)            -> wxControlWithItems;
super(wxCommandEvent)        -> wxEvent;
super(wxContextMenuEvent)    -> wxCommandEvent;
super(wxControl)             -> wxWindow;
super(wxControlWithItems)    -> wxControl;
super(wxCursor)              -> wxBitmap;
super(wxDateEvent)           -> wxCommandEvent;
super(wxDatePickerCtrl)      -> wxPickerBase;
super(wxDialog)              -> wxTopLevelWindow;
super(wxDirDialog)           -> wxDialog;
super(wxDirPickerCtrl)       -> wxPickerBase;
super(wxDisplayChangedEvent) -> wxEvent;
super(wxEraseEvent)          -> wxEvent;
super(wxFileDataObject)      -> wxDataObject;
super(wxFileDialog)          -> wxDialog;
super(wxFileDirPickerEvent)  -> wxCommandEvent;
super(wxFilePickerCtrl)      -> wxPickerBase;
super(wxFindReplaceDialog)   -> wxDialog;
super(wxFlexGridSizer)       -> wxGridSizer;
super(wxFocusEvent)          -> wxEvent;
super(wxFontDialog)          -> wxDialog;
super(wxFontPickerCtrl)      -> wxPickerBase;
super(wxFontPickerEvent)     -> wxCommandEvent;
super(wxFrame)               -> wxTopLevelWindow;
super(wxGBSizerItem)         -> wxSizerItem;
super(wxGLCanvas)            -> wxWindow;
super(wxGauge)               -> wxControl;
super(wxGenericDirCtrl)      -> wxControl;
super(wxGraphicsBrush)       -> wxGraphicsObject;
super(wxGraphicsContext)     -> wxGraphicsObject;
super(wxGraphicsFont)        -> wxGraphicsObject;
super(wxGraphicsMatrix)      -> wxGraphicsObject;
super(wxGraphicsPath)        -> wxGraphicsObject;
super(wxGraphicsPen)         -> wxGraphicsObject;
super(wxGrid)                -> wxScrolledWindow;
super(wxGridBagSizer)        -> wxFlexGridSizer;
super(wxGridEvent)           -> wxNotifyEvent;
super(wxGridSizer)           -> wxSizer;
super(wxHelpEvent)           -> wxEvent;
super(wxIcon)                -> wxBitmap;
super(wxIconizeEvent)        -> wxEvent;
super(wxIdleEvent)           -> wxEvent;
super(wxJoystickEvent)       -> wxEvent;
super(wxKeyEvent)            -> wxEvent;
super(wxListBox)             -> wxControlWithItems;
super(wxListCtrl)            -> wxControl;
super(wxListEvent)           -> wxNotifyEvent;
super(wxListView)            -> wxControl;
super(wxMDIChildFrame)       -> wxFrame;
super(wxMDIChildWindow)      -> wxWindow;
super(wxMDIParentFrame)      -> wxFrame;
super(wxMaximizeEvent)       -> wxEvent;
super(wxMemoryDC)            -> wxDC;
super(wxMenu)                -> wxEvtHandler;
super(wxMenuBar)             -> wxWindow;
super(wxMenuEvent)           -> wxEvent;
super(wxMessageDialog)       -> wxDialog;
super(wxMiniFrame)           -> wxFrame;
super(wxMirrorDC)            -> wxDC;
super(wxMouseCaptureChangedEvent) -> wxEvent;
super(wxMouseEvent)          -> wxEvent;
super(wxMoveEvent)           -> wxEvent;
super(wxMultiChoiceDialog)   -> wxDialog;
super(wxNavigationKeyEvent)  -> wxEvent;
super(wxNcPaintEvent)        -> wxEvent;

%% COMPLETE TO HERE
super(wxNotebook)            -> wxControl;
super(wxPaintDC)             -> wxWindowDC;
super(wxPanel)               -> wxWindow;
super(wxPreviewCanvas)       -> wxScrolledWindow;
super(wxProgressDialog)      -> wxDialog;
super(wxRadioBox)            -> wxControlWithItems;
super(wxRadioButton)         -> wxControl;
super(wxScrollBar)           -> wxControl;
super(wxScrolledWindow)      -> wxPanel;
super(wxSlider)              -> wxControl;
super(wxSpinCtrl)            -> wxControl;
super(wxSplashScreen)        -> wxFrame;
super(wxStaticBox)           -> wxControl;
super(wxStaticLine)          -> wxControl;
super(wxStaticText)          -> wxControl;
super(wxStatusBar)           -> wxWindow;
super(wxStyledTextCtrl)      -> wxControl;
super(wxTextCtrl)            -> wxControl;
super(wxTextEntryDialog)     -> wxDialog;
super(wxToggleButton)        -> wxControl;
super(wxToolBar)             -> wxControl;
super(wxTopLevelWindow)      -> wxWindow;
super(wxTreeCtrl)            -> wxControl;
super(wxWindow)              -> wxEvtHandler;
super(wxWindowDC)            -> wxDC;
super(_)                     -> undefined.


% color/3
color(R, G, B) when R >= 0, R < 256, G >= 0, G < 256, B >= 0, B < 256 -> 
	{R, G, B, 255};
color(_, _, _) -> 
	undefined.
	
% color/2
color(Name, Alpha) when is_atom(Name), Alpha >= 0, Alpha < 256 ->
	{R, G, B, _} = color(Name), 
	{R, G, B, Alpha};
color(_, _) -> 
	undefined.

% color/1
color(Value) when is_integer(Value) -> 
	color(Value bsr 16, (Value bsr 8) band 16#ff, Value band 16#ff);
color([$#, R, G, B]) -> 
	color(erlang:list_to_integer([R, R, G, G, B, B], 16));
color([$#, R, R1, G, G1, B, B1]) ->
	color(erlang:list_to_integer([R, R1, G, G1, B, B1], 16));
color([R, G, B]) -> 
	color(R, G, B);
color([R, G, B, A]) -> 
	{R, G, B, A};
%% ATTRIBUTE COLOR NAMES
%% HTML 4: aqua, black, blue, fuchsia, gray, green, lime, maroon, 
%%         navy, olive, purple, red, silver, teal, white, yellow
color(aliceblue) -> color(16#f0f8ff);
color(antiquewhite) -> color(16#faebd7);
color(aqua) -> color(16#00ffff);
color(aquamarine) -> color(16#7fffd4);
color(azure) -> color(16#f0ffff);
color(beige) -> color(16#f5f5dc);
color(bisque) -> color(16#ffe4c4);
color(black) -> color(16#000000);
color(blanchedalmond) -> color(16#ffebcd);
color(blue) -> color(16#0000ff);
color(blueviolet) -> color(16#8a2be2);
color(brown) -> color(16#a52a2a);
color(burlywood) -> color(16#deb887);
color(cadetblue) -> color(16#5f9ea0);
color(chartreuse) -> color(16#7fff00);
color(chocolate) -> color(16#d2691e);
color(coral) -> color(16#ff7f50);
color(cornflowerblue) -> color(16#6495ed);
color(cornsilk) -> color(16#fff8dc);
color(crimson) -> color(16#dc143c);
color(cyan) -> color(16#00ffff);
color(darkblue) -> color(16#00008b);
color(darkcyan) -> color(16#008b8b);
color(darkgoldenrod) -> color(16#b8860b);
color(darkgray) -> color(16#a9a9a9);
color(darkgrey) -> color(16#a9a9a9);
color(darkgreen) -> color(16#006400);
color(darkkhaki) -> color(16#bdb76b);
color(darkmagenta) -> color(16#8b008b);
color(darkolivegreen) -> color(16#556b2f);
color(darkorange) -> color(16#ff8c00);
color(darkorchid) -> color(16#9932cc);
color(darkred) -> color(16#8b0000);
color(darksalmon) -> color(16#e9967a);
color(darkseagreen) -> color(16#8fbc8f);
color(darkslateblue) -> color(16#483d8b);
color(darkslategray) -> color(16#2f4f4f);
color(darkturquoise) -> color(16#00ced1);
color(darkviolet) -> color(16#9400d3);
color(deeppink) -> color(16#ff1493);
color(deepskyblue) -> color(16#00bfff);
color(dimgray) -> color(16#696969);
color(dodgerblue) -> color(16#1e90ff);
color(firebrick) -> color(16#b22222);
color(floralwhite) -> color(16#fffaf0);
color(forestgreen) -> color(16#228b22);
color(fuchsia) -> color(16#ff00ff);
color(gainsboro) -> color(16#dcdcdc);
color(ghostwhite) -> color(16#f8f8ff);
color(gold) -> color(16#ffd700);
color(goldenrod) -> color(16#daa520);
color(gray) -> color(16#808080);
color(grey) -> color(16#808080);
color(green) -> color(16#008000);
color(greenyellow) -> color(16#adff2f);
color(honeydew) -> color(16#f0fff0);
color(hotpink) -> color(16#ff69b4);
color(indianred ) -> color(16#cd5c5c);
color(indigo ) -> color(16#4b0082);
color(ivory) -> color(16#fffff0);
color(khaki) -> color(16#f0e68c);
color(lavender) -> color(16#e6e6fa);
color(lavenderblush) -> color(16#fff0f5);
color(lawngreen) -> color(16#7cfc00);
color(lemonchiffon) -> color(16#fffacd);
color(lightblue) -> color(16#add8e6);
color(lightcoral) -> color(16#f08080);
color(lightcyan) -> color(16#e0ffff);
color(lightgoldenrodyellow) -> color(16#fafad2);
color(lightgray) -> color(16#d3d3d3);
color(lightgrey) -> color(16#d3d3d3);
color(lightgreen) -> color(16#90ee90);
color(lightpink) -> color(16#ffb6c1);
color(lightsalmon) -> color(16#ffa07a);
color(lightseagreen) -> color(16#20b2aa);
color(lightskyblue) -> color(16#87cefa);
color(lightslategray) -> color(16#778899);
color(lightsteelblue) -> color(16#b0c4de);
color(lightyellow) -> color(16#ffffe0);
color(lime) -> color(16#00ff00);
color(limegreen) -> color(16#32cd32);
color(linen) -> color(16#faf0e6);
color(magenta) -> color(16#ff00ff);
color(maroon) -> color(16#800000);
color(mediumaquamarine) -> color(16#66cdaa);
color(mediumblue) -> color(16#0000cd);
color(mediumorchid) -> color(16#ba55d3);
color(mediumpurple) -> color(16#9370d8);
color(mediumseagreen) -> color(16#3cb371);
color(mediumslateblue) -> color(16#7b68ee);
color(mediumspringgreen) -> color(16#00fa9a);
color(mediumturquoise) -> color(16#48d1cc);
color(mediumvioletred) -> color(16#c71585);
color(midnightblue) -> color(16#191970);
color(mintcream) -> color(16#f5fffa);
color(mistyrose) -> color(16#ffe4e1);
color(moccasin) -> color(16#ffe4b5);
color(navajowhite) -> color(16#ffdead);
color(navy) -> color(16#000080);
color(oldlace) -> color(16#fdf5e6);
color(olive) -> color(16#808000);
color(olivedrab) -> color(16#6b8e23);
color(orange) -> color(16#ffa500);
color(orangered) -> color(16#ff4500);
color(orchid) -> color(16#da70d6);
color(palegoldenrod) -> color(16#eee8aa);
color(palegreen) -> color(16#98fb98);
color(paleturquoise) -> color(16#afeeee);
color(palevioletred) -> color(16#d87093);
color(papayawhip) -> color(16#ffefd5);
color(peachpuff) -> color(16#ffdab9);
color(peru) -> color(16#cd853f);
color(pink) -> color(16#ffc0cb);
color(plum) -> color(16#dda0dd);
color(powderblue) -> color(16#b0e0e6);
color(purple) -> color(16#800080);
color(red) -> color(16#ff0000);
color(rosybrown) -> color(16#bc8f8f);
color(royalblue) -> color(16#4169e1);
color(saddlebrown) -> color(16#8b4513);
color(salmon) -> color(16#fa8072);
color(sandybrown) -> color(16#f4a460);
color(seagreen) -> color(16#2e8b57);
color(seashell) -> color(16#fff5ee);
color(sienna) -> color(16#a0522d);
color(silver) -> color(16#c0c0c0);
color(skyblue) -> color(16#87ceeb);
color(slateblue) -> color(16#6a5acd);
color(slategray) -> color(16#708090);
color(slategrey) -> color(16#708090);
color(snow) -> color(16#fffafa);
color(springgreen) -> color(16#00ff7f);
color(steelblue) -> color(16#4682b4);
color(tan) -> color(16#d2b48c);
color(teal) -> color(16#008080);
color(thistle) -> color(16#d8bfd8);
color(tomato) -> color(16#ff6347);
color(turquoise) -> color(16#40e0d0);
color(violet) -> color(16#ee82ee);
color(wheat) -> color(16#f5deb3);
color(white) -> color(16#ffffff);
color(whitesmoke) -> color(16#f5f5f5);
color(yellow) -> color(16#ffff00);
color(yellowgreen) -> color(16#9acd32);
color(_) -> undefined.
