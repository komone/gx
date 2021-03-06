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

-module(gx_ui_list).

-include("gx.hrl").
-include("gx_wx.hrl").

-behaviour(gx_ui).
-export([mapping_info/0, create/3, read/3, config/4]).

%% wxListView rather than wxListBox?
mapping_info() -> [
	#gx_wx{
		type = list, 
		attributes = record_info(fields, list), 
		extends = gx_ui_controlwithitems,
		wx_type = wxListBox,
		event_map = [
				{ondblclick, command_listbox_doubleclicked},
				{onselect, command_listbox_selected}
		]
	}].

%%
create(Gx, Parent, L = #list{id = GxName, size = _Size, choices = Items, selected = Selected}) ->
	Opts = {options, [pos, size, choices, style, validator], [{choices, Items}]},
	Ref = gx_wx:call(Gx, ?wxListBox_new_3, [Parent, -1, Opts]),
	case in_range(Selected, 0, length(Items)) of
	true ->
		gx_wx:cast(Gx, ?wxControlWithItems_SetSelection, [Ref, Selected]);
	false ->
		ignore
	end,
	ok = gx_ui_control:init(Gx, Parent, L#list{ref = Ref}, mapping_info()),
	#gx_ui{id = GxName, ref = Ref, parent = Parent}.
	
%%
read(G, Ref, Key) ->
	gx_ui_controlwithitems:read(G, Ref, Key).

%%
config(G, Ref, Key, Value) ->
	gx_ui_controlwithitems:config(G, Ref, Key, Value).

%
in_range(Selected, Min, Max) ->
	is_integer(Selected) andalso Selected >= Min andalso Selected < Max.
