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

-module(gx_ui_menu).

-include("gx.hrl").
-include("gx_wx.hrl").

-behaviour(gx_ui).
-export([mapping_info/0, create/3, read/3, config/4]).

-export([context_menu/2]).

%%
mapping_info() -> [
	#gx_wx{
		type = menu, 
		attributes = record_info(fields, menu), 
		extends = gx_ui_control,
		wx_type = wxMenu,
		event_map = [{onselect, command_menu_selected}]
	}].

%% Opts = [{label, String}]
create(Gx, Parent = #wx_ref{type = ParentType}, #menu{id = GxName, label = Label, icon = Icon, content = Children}) ->
%	Menu = wxMenu:new(),
%	MenuItem = wxMenuItem:new([{parentMenu, Parent}, {id, Command}, {text, binary_to_list(Label)}, {kind, ?wxITEM_NORMAL}, {subMenu, Menu}]),
	Ref = gx_wx:call(Gx, ?wxMenu_new_1, [{options, [style], []}]),
	case ParentType of
	wxMenuBar ->
		true = gx_wx:call(Gx, ?wxMenuBar_Append, [Parent, Ref, Label]);
	wxMenu ->
		ItemRef = gx_wx:call(Gx, ?wxMenu_Append_4_1, [Parent, -1, Label, Ref, {options, [help], []}]),
	%	?TTY({Ref, ItemRef}),
		set_submenu_menuitem_icon(Gx, ItemRef, Icon);
	_ -> 
		gx_ui:bind_event(Gx, Parent, {right_up, {context_menu, GxName, Ref}}),
		gx_ui:bind_event(Gx, Ref, {command_menu_selected, {GxName, command}})
	end,
	gx_ui:create(Gx, Ref, Children),
	#gx_ui{id = GxName, ref = Ref, parent = Parent}.

%%
read(Gx, Ref = #wx_ref{type = wxMenu}, Key) ->
	gx_ui_control:read(Gx, Ref, Key).

config(Gx, Ref, Key, Value) ->
	gx_ui_control:config(Gx, Ref, Key, Value).

%% move to gx_ui_window?
context_menu(Gx, #gx{ref = Parent, event = Ref = #wx_ref{type = wxMenu}}) ->
	%instanceof wxWindow...
	true = gx_wx:call(Gx, ?wxWindow_PopupMenu_2, [Parent, Ref, {options, [pos], []}]),
	ok.

%% This doesn't even work in wxWidgets... yet
set_submenu_menuitem_icon(_, _, undefined) ->
	ok;
set_submenu_menuitem_icon(Gx, MenuItem, Icon) ->
	case gx_cache:get_bitmap(Gx, Icon) of
	undefined ->
		?TTY({not_found, Icon});
	{ok, WxBitmap} ->
		gx_wx:cast(Gx, ?wxMenuItem_SetBitmap, [MenuItem, WxBitmap])
	end.
