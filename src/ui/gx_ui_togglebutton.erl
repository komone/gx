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


-module(gx_ui_togglebutton).

-include("gx.hrl").
-include("gx_wx.hrl").

-behaviour(gx_ui).
-export([mapping_info/0, create/3, read/3, config/4]).

%%
mapping_info() -> [
	#gx_wx{
		type = togglebutton, 
		attributes = record_info(fields, togglebutton), 
		extends = gx_ui_control,
		wx_type = wxToggleButton,
		event_map = []
	}].

%%
create(Gx, Parent = #wx_ref{}, T = #togglebutton{id = GxName, label = Label}) -> 
	Ref = gx_wx:call(Gx, ?wxToggleButton_new_4, [Parent, -1, Label, 
		{options, [pos, size, style, validator], [{style, ?wxBORDER_STATIC}]}]),
	ok = gx_ui_control:init(Gx, Parent, T#togglebutton{ref = Ref}, mapping_info()),
	#gx_ui{id = GxName, ref = Ref, parent = Parent}.
	
%%
read(G, Ref, Key) ->
	gx_ui_control:read(G, Ref, Key).

%%
config(G, Ref, Key, Value) ->
	gx_ui_control:config(G, Ref, Key, Value).
