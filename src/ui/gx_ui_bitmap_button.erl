%% Copyright 2010-2014 Steve Davis <steve@simulacity.com>
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

-module(gx_ui_bitmap_button).
%-define(wxBitmapButton_new_4, 1475).
%-define(wxBitmapButton_new_0, 1476).
%-define(wxBitmapButton_Create, 1477).
%-define(wxBitmapButton_GetBitmapDisabled, 1478).
%-define(wxBitmapButton_GetBitmapFocus, 1480).
%-define(wxBitmapButton_GetBitmapLabel, 1482).
%-define(wxBitmapButton_GetBitmapSelected, 1484).
%-define(wxBitmapButton_SetBitmapDisabled, 1486).
%-define(wxBitmapButton_SetBitmapFocus, 1487).
%-define(wxBitmapButton_SetBitmapLabel, 1488).
%-define(wxBitmapButton_SetBitmapSelected, 1489).
%-define(wxBitmapButton_destroy, 1490).

-include("gx.hrl").
-include("gx_wx.hrl").

-behaviour(gx_ui).
-export([mapping_info/0, create/3, read/3, config/4]).

mapping_info() -> [
	#gx_wx{
		type = bitmapbutton, 
		attributes = record_info(fields, bitmapbutton), 
		extends = gx_ui_button,
		wx_type = wxBitmapButton,
		event_map = [{onclick, command_button_clicked}]
	}].

%
create(G, Parent, B = #bitmapbutton{id = GxName, label = _Label, icon = Icon}) ->
	{ok, WxBitmap} = gx_cache:load_image(G, Icon),
	Opts = {options, [pos, size, style, validator], []},
	Ref = #wx_ref{} = gx_wx:call(G, ?wxBitmapButton_new_4, Parent, -1, WxBitmap, Opts),
	ok = gx_ui_control:init(G, Parent, B#bitmapbutton{ref = Ref}, mapping_info()),
	#gx_ui{id = GxName, ref = Ref, parent = Parent}.

read(G, Ref, icon) ->
	gx_wx:call(G, ?wxBitmapButton_GetBitmapLabel, [Ref]);
read(G, Ref, disabled_icon) ->
	gx_wx:call(G, ?wxBitmapButton_GetBitmapDisabled, [Ref]);
read(G, Ref, focused_icon) ->
	gx_wx:call(G, ?wxBitmapButton_GetBitmapFocus, [Ref]);
read(G, Ref, selected_icon) ->
	gx_wx:call(G, ?wxBitmapButton_GetBitmapSelected, [Ref]);
read(G, Ref, Key) ->
	gx_ui_button:read(G, Ref, Key).

config(G, Ref, K, V) ->
	gx_ui_button:config(G, Ref, K, V).
