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

-module(gx_ui_checkbox).

-include("gx.hrl").
-include_lib("gx_wx.hrl").

-behaviour(gx_ui).
-export([mapping_info/0, create/3, read/3, config/4]).

%%
mapping_info() -> [
	#gx_wx{
		type = checkbox, 
		attributes = record_info(fields, checkbox), 
		extends = gx_ui_control,
		wx_type = wxCheckBox,
		event_map = [{onselect, command_checkbox_clicked}]
	}].
%%
create(Gx, Parent, C = #checkbox{id = GxName, label = Label, value = Value}) ->
	Style = get_style(C),
	Ref = gx_wx:call(Gx, ?wxCheckBox_new_4, [Parent, -1, Label, 
		{options, [pos, size, style, validator], [{style, Style}]}]),
	ok = gx_ui_control:init(Gx, Parent, C#checkbox{ref = Ref}, mapping_info()),
	config(Gx, Ref, value, Value),
	#gx_ui{id = GxName, ref = Ref, parent = Parent}.

%%
read(Gx, Ref, value) ->
	gx_wx:call(Gx, ?wxCheckBox_GetValue, [Ref]);
read(Gx, Ref, Key) ->
	gx_ui_control:read(Gx, Ref, Key).
%%
config(Gx, Ref, value, Value) ->
	gx_wx:cast(Gx, ?wxCheckBox_SetValue, [Ref, Value]);
config(G, Ref, Key, Value) ->
	gx_ui_control:config(G, Ref, Key, Value).

%% TODO: 3-state checkboxes not yet supported
get_style(#checkbox{justify = right}) ->
	?wxCHK_2STATE bor ?wxALIGN_RIGHT;
get_style(#checkbox{}) ->
	?wxCHK_2STATE.

