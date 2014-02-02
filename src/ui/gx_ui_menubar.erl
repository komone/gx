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

-module(gx_ui_menubar).

-include("gx.hrl").
-include("gx_wx.hrl").

-behaviour(gx_ui).
-export([mapping_info/0, create/3, read/3, config/4]).

%%
mapping_info() -> [
	#gx_wx{
		type = menubar, 
		attributes = record_info(fields, menubar), 
		extends = gx_ui_window,
		wx_type = wxMenuBar,
		event_map = [{onselect, command_menu_selected}]
	}].

%%
create(Gx, Parent, #menubar{id = GxName, content = Children}) ->
	Ref = gx_wx:call(Gx, ?wxMenuBar_new_0, []),
%	ExistingRef = gx_wx:call(Gx, ?wxFrame_GetMenuBar, [Parent]),
%	Count = gx_wx:call(Gx, ?wxMenuBar_GetMenuCount, [ExistingRef]),
%	?TTY({ExistingRef, Count}),
	gx_wx:cast(Gx, ?wxFrame_SetMenuBar, [Parent, Ref]),
	
	%% TODO: there are more menubar events to implement ?
	gx_ui:bind_event(Gx, Parent, {command_menu_selected, {GxName, command}}),
	gx_ui:create(Gx, Ref, Children),
	#gx_ui{id = GxName, ref = Ref, parent = Parent}.

%%
read(Gx, Ref = #wx_ref{type = wxMenuBar}, Key) ->
	gx_ui_window:read(Gx, Ref, Key).
	
%%
config(Gx, Ref, Key, Value) ->
	gx_ui_window:config(Gx, Ref, Key, Value).

