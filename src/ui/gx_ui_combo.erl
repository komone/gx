%% Copyright 2010 Steve Davis <steve@simulacity.com>
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

-module(gx_ui_combo).

-include("gx.hrl").
-include("gx_wx.hrl").

-behaviour(gx_ui).
-export([mapping_info/0, create/3, read/3, config/4]).

%%
mapping_info() -> [
	#gx_wx{
		type = combo, 
		attributes = record_info(fields, combo), 
		extends = gx_ui_control,
		wx_type = wxComboBox,
		event_map = [
			{onsubmit, command_text_enter}, % BUG: not working?
			{onchange, command_text_updated},
			{onselect, command_combobox_selected}
		]
	}].

%%
create(Gx, Parent, C = #combo{id = GxName, choices = Items, selected = Selected}) ->
	case is_valid_selection(Selected, length(Items)) of
	true ->
		Value = lists:nth(Selected + 1, Items);
	false ->
		Value = <<"">>
	end,
	Ref = gx_wx:call(Gx, ?wxComboBox_new_3, [Parent, -1, 
		{options, [value, pos, size, choices, style, validator], [{value, Value}, {choices, Items}]}]),
	ok = gx_ui_control:init(Gx, Parent, C#combo{ref = Ref}, mapping_info()),
	#gx_ui{id = GxName, ref = Ref, parent = Parent}.

is_valid_selection(Selected, Max) ->
	is_integer(Selected) andalso Selected >= 0 andalso Selected < Max.

read(Gx, Ref = #wx_ref{}, value) ->
	gx_wx:cast(Gx, ?wxComboBox_GetValue, [Ref]);
read(Gx, Ref, Key) ->
	gx_ui_control:read(Gx, Ref, Key).
	
config(Gx, Ref = #wx_ref{}, value, Text) ->
	gx_wx:cast(Gx, ?wxComboBox_SetValue, [Ref, Text]);
config(Gx, Ref, Key, Value) ->
	gx_ui_control:config(Gx, Ref, Key, Value).
