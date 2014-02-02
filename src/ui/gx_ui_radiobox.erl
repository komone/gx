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

-module(gx_ui_radiobox).

-include("gx.hrl").
-include("gx_wx.hrl").

-behaviour(gx_ui).
-export([mapping_info/0, create/3, read/3, config/4]).

%%
mapping_info() -> [
	#gx_wx{
		type = radiobox, 
		attributes = record_info(fields, radiobox), 
		extends = gx_ui_control,
		wx_type = wxRadioBox,
		event_map = [{onchange, command_radiobox_selected}]
	}].

%%
create(Gx, Parent, R = #radiobox{id = GxName, label = Label, selected = Selected, choices = Choices}) ->
	Ref = gx_wx:call(Gx, ?wxRadioBox_new, [Parent, -1, Label, {-1, -1}, {-1, -1}, Choices, 
		{options, [majorDim, style, val], []}]),
	case in_range(Selected, 0, length(Choices)) of
	true ->
		gx_wx:cast(Gx, ?wxRadioBox_SetSelection, [Ref, Selected]);
	false ->
		ignore
	end,
	ok = gx_ui_control:init(Gx, Parent, R#radiobox{ref = Ref}, mapping_info()),
	#gx_ui{id = GxName, ref = Ref, parent = Parent}.


%%
read(Gx, Ref = #wx_ref{type = wxRadioBox}, value) ->
	_Item = gx_wx:call(Gx, ?wxRadioBox_GetSelection, [Ref]);
read(G, Ref, Key) ->
	gx_ui_control:read(G, Ref, Key).

%%
config(G, Ref, Key, Value) ->
	gx_ui_control:config(G, Ref, Key, Value).

in_range(Selected, Min, Max) ->
	is_integer(Selected) andalso Selected >= Min andalso Selected < Max.
