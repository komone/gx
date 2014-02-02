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

-module(gx_ui_button).

-include("gx.hrl").
-include("gx_wx.hrl").

-behaviour(gx_ui).
-export([mapping_info/0, create/3, read/3, config/4]).

%%
mapping_info() -> [
	#gx_wx{
		type = button, 
		attributes = record_info(fields, button), 
		extends = gx_ui_control,
		wx_type = wxButton,
		event_map = [{onclick, command_button_clicked}]
	}].
	
%% 
create(G, Parent, B = #button{id = GxName, label = Label, icon = undefined}) when is_binary(Label) ->		
	Args = [Parent, -1, {options, [label, pos, size, style, validator], [{label, Label}]}],
	Ref = #wx_ref{} = gx_wx:call(G, ?wxButton_new_3, Args),
	ok = gx_ui_control:init(G, Parent, B#button{ref = Ref}, mapping_info()),
	#gx_ui{id = GxName, ref = Ref, parent = Parent}.

%%
read(Gx, _Ref, default_size) ->
	{_, _} = gx_wx:call(Gx, ?wxButton_GetDefaultSize, []);
%% wxButton_GetLabel isn't exported but should be...?
read(Gx, Ref, Key) ->
	gx_ui_control:read(Gx, Ref, Key).

%%
config(Gx, Ref, default, true) ->
	gx_wx:cast(Gx, ?wxButton_SetDefault, [Ref]);
config(Gx, Ref, label, Label) when is_binary(Label) ->
	gx_wx:cast(Gx, ?wxButton_SetLabel, [Ref, Label]);
config(Gx, Ref, K, V) ->
	gx_ui_control:config(Gx, Ref, K, V).

