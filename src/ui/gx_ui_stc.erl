%% Copyright 2010-2011 Steve Davis <steve@simulacity.com>
%
% Licensed under the Apache License, Version 2.0 (the "License");
% you may not use this file except in compliance with the License.
% You may obtain a copy of the License at
% 
% http://www.apache.org/licenses/LICENSE-2.0
% 
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS,
% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
% See the License for the specific language governing permissions and
% limitations under the License.

-module(gx_ui_stc).

-include("gx.hrl").
-include("gx_wx.hrl").

-behaviour(gx_ui).
-export([mapping_info/0, create/3, read/3, config/4]).


%%
mapping_info() -> [
	#gx_wx{
		type = editor, 
		attributes = record_info(fields, editor), 
		extends = gx_ui_control,
		wx_type = wxStyledTextCtrl,
		event_map = [
%			stc_autocomp_selection,
%			stc_calltip_click, 
%			stc_change, 
			{onchange, stc_charadded}
%			stc_do_drop, 
%			stc_doubleclick, 
%			stc_drag_over, 
%			stc_dwellend, 
%			stc_dwellstart, 
%			stc_hotspot_click, 
%			stc_hotspot_dclick, 
%			stc_key, 
%			stc_macrorecord, 
%			stc_marginclick, 
%			stc_modified, 
%			stc_needshown, 
%			stc_painted, 
%			stc_romodifyattempt, 
%			stc_savepointleft, 
%			stc_savepointreached, 
%			stc_start_drag, 
%			stc_styleneeded, 
%			stc_updateui, 
%			stc_uridropped, 
%			stc_userlistselection, 
%			stc_zoom
		]
	}].

%% TODO: Add features! Opts = [{label, String}]
create(Gx, Parent, E = #editor{id = GxName, lexer = Lexer}) ->
	Ref = gx_wx:call(Gx, ?wxStyledTextCtrl_new_2, [Parent, {options, [id, pos, size, style], []}]),
	set_style(Gx, Ref, ?wxSTC_STYLE_DEFAULT, <<"face:SourceCodePro-Regular,size:11,back:#ffffff,fore:#000000">>),
	% NOTE: should we use ?wxStyledTextCtrl_ClearDocumentStyle or ?wxStyledTextCtrl_StyleClearAll ...?
	{ok, Lexer} = gx_ui_stc_lexer:init(Gx, Ref, Lexer),
	
%% MARGINS
%	gx_wx:cast(Port, ?wxStyledTextCtrl_SetMarginType, [Ref, 0, ?wxSTC_MARGIN_NUMBER]),  
	LineWidth = gx_wx:call(Gx, ?wxStyledTextCtrl_TextWidth, [Ref, ?wxSTC_STYLE_LINENUMBER, <<"_9999">>]),
%	gx_wx:cast(Port, ?wxStyledTextCtrl_SetMarginType, [Ref, 0, LineWidth]),  	
	gx_wx:cast(Gx, ?wxStyledTextCtrl_SetMarginWidth, [Ref, 0, LineWidth]),
	gx_wx:cast(Gx, ?wxStyledTextCtrl_SetMarginWidth, [Ref, 1, 8]),  

%% TABS
%	wxStyledTextCtrl:setUseTabs(Ref, true),
	gx_wx:cast(Gx, ?wxStyledTextCtrl_SetTabWidth, [Ref, 4]),

%% INDENTS
%	wxStyledTextCtrl:setIndent(Ref, 4),
%	wxStyledTextCtrl:setTabIndents(Ref, true),
%	wxStyledTextCtrl:setBackSpaceUnIndents(Ref, true),
	set_style(Gx, Ref, ?wxSTC_STYLE_INDENTGUIDE, <<"fore:#c0c0c0">>),
	gx_wx:cast(Gx, ?wxStyledTextCtrl_SetIndentationGuides, [Ref, true]),
	gx_wx:cast(Gx, ?wxStyledTextCtrl_SetMarginWidth, [Ref, 1, 18]),
	{ok, BitmapRef} = gx_cache:get_bitmap(Gx, <<"warning.gif">>),
	gx_wx:cast(Gx, ?wxStyledTextCtrl_MarkerDefineBitmap, [Ref, 1, BitmapRef]),
	{ok, BitmapRef0} = gx_cache:get_bitmap(Gx, <<"error.gif">>),
	gx_wx:cast(Gx, ?wxStyledTextCtrl_MarkerDefineBitmap, [Ref, 2, BitmapRef0]),
	ok = gx_ui:bind_event(Gx, Ref, {stc_styleneeded, {GxName, local}}),

	ok = gx_ui_control:init(Gx, Parent, E#editor{ref = Ref, fill = both}, mapping_info()),
	#gx_ui{id = GxName, ref = Ref, parent = Parent}.

%%
read(Gx, Ref, lexer) ->
	gx_wx:call(Gx, ?wxStyledTextCtrl_GetLexer, [Ref]);
read(Gx, Ref, bar) ->
	Mode = gx_wx:call(Gx, ?wxStyledTextCtrl_GetEdgeMode, [Ref]),
	Column = gx_wx:call(Gx, ?wxStyledTextCtrl_GetEdgeColumn, [Ref]),
	Color = gx_wx:call(Gx, ?wxStyledTextCtrl_GetEdgeColour, [Ref]),
	{bar, Mode, Column, Color};
read(Gx, Ref, tabs) ->
	gx_wx:call(Gx, ?wxStyledTextCtrl_GetTabWidth, [Ref]);
read(Gx, Ref, text) ->
	list_to_binary(gx_wx:call(Gx, ?wxStyledTextCtrl_GetText, [Ref]));
read(Gx, Ref, selected) ->
	list_to_binary(gx_wx:call(Gx, ?wxStyledTextCtrl_GetSelectedText, [Ref]));
read(Gx, Ref, margin) -> 
	[{margin, X, [
		{type, gx_wx:call(Gx, ?wxStyledTextCtrl_GetMarginType, [Ref, X])}, 
		{width, gx_wx:call(Gx, ?wxStyledTextCtrl_GetMarginWidth, [Ref, X])},
		{mask, gx_wx:call(Gx, ?wxStyledTextCtrl_GetMarginMask, [Ref, X])},
		{clickable, gx_wx:call(Gx, ?wxStyledTextCtrl_GetMarginSensitive, [Ref, X])}
	]} || X <- [0,1,2,3,4]];
read(Gx, Ref, style) ->
	Pos = gx_wx:call(Gx, ?wxStyledTextCtrl_GetCurrentPos, [Ref]),
	Style = gx_wx:call(Gx, ?wxStyledTextCtrl_GetStyleAt, [Ref, Pos]),

	{Pos, Style};
read(Gx, Ref, Key) ->
	gx_ui_control:read(Gx, Ref, Key).

%%
% wx 2.9 only
%config(Gx, Ref, annotation, {Line, Text}) ->
%	gx_wx:cast(Gx, ?wxStyledTextCtrl_AnnotationSetText, [Ref, Line, Text]);
config(Gx, Ref, autocomplete, List) when is_binary(List) ->
	gx_wx:cast(Gx, ?wxStyledTextCtrl_AutoCompStops, [Ref, <<",.">>]),
	gx_wx:cast(Gx, ?wxStyledTextCtrl_AutoCompShow, [Ref, 0, List]);
config(Gx, Ref, autocomplete, List) when is_list(List) ->
	gx_wx:cast(Gx, ?wxStyledTextCtrl_AutoCompStops, [Ref, <<",.">>]),
	gx_wx:cast(Gx, ?wxStyledTextCtrl_AutoCompShow, [Ref, 0, autocomplete_list(List)]);
config(Gx, Ref, commandkey, {Modifier, Key, Command}) ->
	set_command_key(Gx, Ref, Key, Modifier, Command);
config(Gx, Ref, load, Value) when is_binary(Value); is_list(Value) ->
	Path = path:new(Value),
	case path:type(Path) of
	regular ->
		gx_wx:call(Gx, ?wxStyledTextCtrl_LoadFile, [Ref, Path]);
	_ ->
		{error, enoent}
	end;
config(Gx, Ref, font, Value = #font{}) ->
	Style = encode_style(Value, <<"back:#ffffff,fore:#000000">>),
	?TTY(Style),
	set_style(Gx, Ref, ?wxSTC_STYLE_DEFAULT, Style);
config(Gx, Ref, tabs, Value) when is_integer(Value) ->
	gx_wx:cast(Gx, ?wxStyledTextCtrl_SetTabWidth, [Ref, Value]);
config(Gx, Ref, text, Value) when is_binary(Value) ->
	gx_wx:cast(Gx, ?wxStyledTextCtrl_SetText, [Ref, Value]);	
config(Gx, Ref, bar, Value) when is_integer(Value) ->
	gx_wx:cast(Gx, ?wxStyledTextCtrl_SetEdgeColumn, [Ref, Value]),
	gx_wx:cast(Gx, ?wxStyledTextCtrl_SetEdgeMode, [Ref, 1]);
config(Gx, Ref, marker, Value) when is_integer(Value) ->
	gx_wx:call(Gx, ?wxStyledTextCtrl_MarkerAdd, [Ref, Value, 2]);
config(Gx, Ref, marker, clear) ->
	gx_wx:cast(Gx, ?wxStyledTextCtrl_MarkerDeleteAll, [Ref, -1]);
config(Gx, Ref, editable, Value) when is_boolean(Value) ->
	ReadOnly = Value =:= false,
	gx_wx:cast(Gx, ?wxStyledTextCtrl_SetReadOnly, [Ref, ReadOnly]);
%TEMP
config(Gx, Ref, bold, {StyleNumber, Value}) when is_boolean(Value) ->
	gx_wx:cast(Gx, ?wxStyledTextCtrl_StyleSetBold, [Ref, StyleNumber, Value]);
config(Gx, Ref, Key, Value) ->
	gx_ui_control:config(Gx, Ref, Key, Value).

encode_style(#font{face = Face, size = Size}, Color) ->
	Size0 = list_to_binary(integer_to_list(Size)),
	<<"face:", Face/binary, ",size:", Size0/binary, ",", Color/binary>>.

autocomplete_list([H|T]) when is_binary(H) ->
	autocomplete_list(T, H);
autocomplete_list([]) ->
	<<>>.
	
autocomplete_list([H|T], Acc) when is_binary(H) ->
	autocomplete_list(T, <<Acc/binary, " ", H/binary>>);
autocomplete_list([], Acc) ->
	Acc.
%
set_style(Gx, Ref, Style, Spec) ->
	gx_wx:cast(Gx, ?wxStyledTextCtrl_StyleSetSpec, [Ref, Style, Spec]).


set_command_key(Gx, Ref, [Key], Modifier, Command) ->
	gx_wx:cast(Gx, ?wxStyledTextCtrl_CmdKeyAssign, [Ref, <<Key, (modifier(Modifier)), (command(Command))>>]).

modifier(control) -> ?wxSTC_SCMOD_CTRL;
modifier(alt) -> ?wxSTC_SCMOD_ALT;
modifier(shift) -> ?wxSTC_SCMOD_SHIFT;
modifier(_) -> ?wxSTC_SCMOD_NORM.

command(cut) -> ?wxSTC_CMD_CUT;
command(copy) -> ?wxSTC_CMD_COPY;
command(paste) -> ?wxSTC_CMD_PASTE;
command(undo) -> ?wxSTC_CMD_UNDO.
%-define(wxSTC_CMD_SELECTALL, 2013).
%-define(wxSTC_CMD_REDO, 2011).
