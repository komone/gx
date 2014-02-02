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

-module(gx_ui_bitmap_button).

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
create(Gx, Parent, B = #bitmapbutton{id = GxName, label = _Label, icon = Icon}) ->
	WxBitmap = gx_ui:get_image(Icon),
	Ref = wxBitmapButton:new(Parent, -1, WxBitmap, []),
	ok = gx_ui_control:init(Gx, Parent, B#bitmapbutton{ref = Ref}, mapping_info()),
	#gx_ui{id = GxName, ref = Ref, parent = Parent}.

read(Gx, Ref, icon) ->
	gx_wx:call(Gx, ?wxBitmapButton_GetBitmapLabel, [Ref]);
read(Gx, Ref, disabled_icon) ->
	gx_wx:call(Gx, ?wxBitmapButton_GetBitmapDisabled, [Ref]);
read(Gx, Ref, focused_icon) ->
	gx_wx:call(Gx, ?wxBitmapButton_GetBitmapFocus, [Ref]);
read(Gx, Ref, selected_icon) ->
	gx_wx:call(Gx, ?wxBitmapButton_GetBitmapSelected, [Ref]);
read(Gx, Ref, Key) ->
	gx_ui_button:read(Gx, Ref, Key).

config(Gx, Ref, K, V) ->
	gx_ui_button:config(Gx, Ref, K, V).

