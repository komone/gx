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

-module(gx_ui_menuitem).

-include("gx.hrl").
-include("gx_wx.hrl").

-behaviour(gx_ui).
-export([mapping_info/0, create/3, read/3, config/4]).

%%
mapping_info() -> [
	#gx_wx{
		type = menuitem, 
		attributes = record_info(fields, menuitem), 
		extends = gx_ui_object,
		wx_type = wxMenuItem,
		event_map = [{onclick, command_button_clicked}]
	}].

%% Opts = [{label, String} | {enable, Bool} | {command, Integer}]
create(Gx, Parent = #wx_ref{type = wxMenu}, #menuitem{
		id = GxName, icon = Icon, type = Type, label = Label, 
		enabled = Enabled, checked = Checked, callbacks = Callbacks}) -> 
	Command = get_menuitem_command(Gx, GxName, Callbacks),
	WxType = get_menuitem_type(Type),
	case GxName of
%	about ->
%		Ref = gx_wx:call(Gx, ?wxMenu_Append_3, [Parent, ?wxID_ABOUT, Label, {options, [help, kind], []} ]);
	_ ->
		Ref = gx_wx:call(Gx, ?wxMenuItem_new, [{options, [parentMenu, id, text, help, kind, subMenu], 
			[{parentMenu, Parent}, {id, Command}, {text, Label}, {kind, WxType}]}]),
		set_icon(Gx, Ref, Icon),
		check_item(Gx, Ref, Type, Checked),
		enable_item(Gx, Ref, Enabled),
		Ref = gx_wx:call(Gx, ?wxMenu_Append_1, [Parent, Ref])
	end,
	%?TTY({Ref, Result}),
	#gx_ui{id = GxName, ref = Ref, parent = Parent}.

read(Gx, Ref = #wx_ref{type = wxMenuItem}, label) ->
	String = gx_wx:call(Gx, ?wxMenuItem_GetLabel, [Ref]),
	list_to_binary(String);
read(Gx, Ref, Key) ->
	gx_ui_object:read(Gx, Ref, Key).

config(Gx, Ref, Key, Value) ->
	gx_ui_object:config(Gx, Ref, Key, Value).

%get_menuitem_command(Gx, about, [{Event, Handler}|_]) ->
%	gx_cache:bind_command(Gx, about, Event, Handler),
%	?wxID_ABOUT;
%get_menuitem_command(Gx, exit, [{Event, Handler}|_]) ->
%	gx_cache:bind_command(Gx, exit, Event, Handler),
%	?wxID_EXIT;
%% TODO: can ignore multiple (but illegal) callbacks here... is this VALID???
get_menuitem_command(Gx, GxName, [{Event, Handler}|_]) ->
	gx_cache:bind_command(Gx, GxName, Event, Handler);
get_menuitem_command(_, _, []) ->
	?wxID_NONE.


get_menuitem_type(radio) ->
	?wxITEM_RADIO;
get_menuitem_type(checkbox) ->
	?wxITEM_CHECK;
%get_menuitem_type(separator) ->
%	?wxITEM_SEPARATOR; %% Not safe...
get_menuitem_type(normal) ->
	?wxITEM_NORMAL.
	
set_icon(_, _, undefined) ->
	ok;
set_icon(Gx, MenuItem, Icon) ->
	case gx_cache:get_bitmap(Gx, Icon) of
	undefined ->
		?TTY({not_found, Icon});
	{ok, WxBitmap} ->
		gx_wx:cast(Gx, ?wxMenuItem_SetBitmap, [MenuItem, WxBitmap])
	end.

check_item(_, _Ref, normal, _Bool) ->
	ok;
check_item(Gx, Ref, _Type, Bool) ->
	gx_wx:cast(Gx, ?wxMenuItem_Check, [Ref, {options, [check], [{check, Bool}]}]).

enable_item(Gx, Ref, Bool) ->
	gx_wx:cast(Gx, ?wxMenuItem_Enable, [Ref, {options, [enable], [{enable, Bool}]}]).
