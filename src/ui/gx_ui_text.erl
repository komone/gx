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

-module(gx_ui_text).

-include("gx.hrl").
-include("gx_wx.hrl").

-behaviour(gx_ui).
-export([mapping_info/0, create/3, read/3, config/4]).

%%
mapping_info() -> [
	#gx_wx{
		type = text, 
		attributes = record_info(fields, text), 
		extends = gx_ui_control,
		wx_type = wxStaticText,
		event_map = []
	}].

%%
create(Gx, Parent, T = #text{id = GxName, label = Label, font = Font, justify = Justify, fixed = Fixed, wrap = Wrap}) ->
	Style = get_justify(Justify) bor get_fixed(Fixed),
	Ref = gx_wx:call(Gx, ?wxStaticText_new_4, [Parent, -1, Label, {options, [pos, size, style], [{style, Style}]}]),
	set_font(Gx, Ref, Font),
	true = is_integer(Wrap),
	ok = gx_wx:cast(Gx, ?wxStaticText_Wrap, [Ref, Wrap]),
% the following seems to be to make mouse clicks ignore this control
%	wxWindow:connect(Ref, right_down, [{skip, true}]),
	ok = gx_ui_control:init(Gx, Parent, T#text{ref = Ref}, mapping_info()),
	#gx_ui{id = GxName, ref = Ref, parent = Parent}.

%%
read(G, Ref, Key) ->
	gx_ui_control:read(G, Ref, Key).

%%
config(Gx, Ref, label, Value) ->
	gx_wx:cast(Gx, ?wxStaticText_SetLabel, [Ref, Value]);
config(Gx, Ref, Key, Value) ->
	gx_ui_control:config(Gx, Ref, Key, Value).

%
get_justify(center) ->
	?wxALIGN_CENTER;
get_justify(right) ->
	?wxALIGN_RIGHT;
get_justify(_) ->
	?wxALIGN_LEFT.

%
get_fixed(true) ->
	?wxST_NO_AUTORESIZE;
get_fixed(_) ->
	0.
	
%
set_font(_, _, undefined) ->
	ok;
set_font(Gx, Ref, <<"bold">>) ->
	FontRef = gx_wx:call(Gx, ?wxFont_new_0, []),	
	gx_wx:cast(Gx, ?wxFont_SetWeight, [FontRef, ?wxFONTWEIGHT_BOLD]),
	gx_wx:call(Gx, ?wxWindow_SetFont, [Ref, FontRef]).
