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

-module(gx_ui_toolbutton).

-include("gx.hrl").
-include("gx_wx.hrl").

-behaviour(gx_ui).
-export([mapping_info/0, create/3, read/3, config/4]).

%%
mapping_info() -> [
	#gx_wx{
		type = toolbutton, 
		attributes = record_info(fields, toolbutton), 
		extends = gx_ui_control,
		wx_type = wxToolButton,
		event_map = []
	}].

%% Opts = [{label, String} | {icon, Path}]
create(Gx, Parent = #wx_ref{type = wxToolBar}, #toolbutton{id = GxName, icon = Icon, label = Label, callbacks = Callbacks}) ->	
	%?TTY(T),
	Command = get_command(Gx, GxName, Callbacks),
	{ok, WxBitmap} = gx_cache:get_bitmap(Gx, Icon),
	Ref = gx_wx:call(Gx, ?wxToolBar_AddTool_3, [Parent, Command, WxBitmap, 
		{options, [shortHelpString, longHelpString], [{shortHelpString, Label}]}]),
	#gx_ui{id = GxName, ref = Ref, parent = Parent}.

%%
read(G, Ref, Key) ->
	gx_ui_control:read(G, Ref, Key).

%%
config(G, Ref, Key, Value) ->
	gx_ui_control:config(G, Ref, Key, Value).

%% TODO: can ignore multiple (but illegal) callbacks here... is this VALID???
get_command(Gx, GxName, [{Event, Handler}|_]) ->
	gx_cache:bind_command(Gx, GxName, Event, Handler);
%	gx_ui:set_command(GxName, [{onselect, command_menu_selected}], Callback);
get_command(_, _, []) ->
	?wxID_NONE.
