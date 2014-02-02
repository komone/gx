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

-module(gx_ui_toolbar).

-include("gx.hrl").
-include("gx_wx.hrl").

-behaviour(gx_ui).
-export([mapping_info/0, create/3, read/3, config/4]).

%%
mapping_info() -> [
	#gx_wx{
		type = toolbar, 
		attributes = record_info(fields, toolbar), 
		extends = gx_ui_control,
		wx_type = wxToolBar,
		event_map = []
	}].

%%
create(Gx, Parent, #toolbar{id = GxName, content = Children}) ->
	_Style = {style, ?wxTB_HORIZONTAL bor ?wxTB_NODIVIDER},
	Ref = gx_wx:call(Gx, ?wxFrame_CreateToolBar, [Parent, {options, [style, id], []}]),
	
	gx_ui:bind_event(Gx, Parent, {command_menu_selected, {GxName, command}}),
% NOTE: These don't appear to work
%	wxToolBar:setToolPacking(Ref, 20), % no effect?
%	gx_wx:cast(Port, ?wxToolBar_SetToolPacking, [Ref, 20]),
%	gx_wx:cast(Gx, ?wxToolBar_SetToolBitmapSize, [Ref, {16, 16}]), % no effect?
	gx_ui:create(Gx, Ref, Children),
	true = gx_wx:call(Gx, ?wxToolBar_Realize, [Ref]),
	#gx_ui{id = GxName, ref = Ref, parent = Parent}.

%%
read(G, Ref, Key) ->
	gx_ui_control:read(G, Ref, Key).

%%
config(G, Ref, Key, Value) ->
	gx_ui_control:config(G, Ref, Key, Value).

