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

-module(gx_ui_aui).

-include("gx.hrl").
-include("gx_wx.hrl").

-behaviour(gx_ui).
-export([mapping_info/0, create/3, read/3, config/4]).

-define(AUI_FLAGS, 
	?wxAUI_MGR_ALLOW_FLOATING 
	bor ?wxAUI_MGR_TRANSPARENT_HINT 
	bor ?wxAUI_MGR_HINT_FADE
	bor ?wxAUI_MGR_TRANSPARENT_DRAG
	bor ?wxAUI_MGR_NO_VENETIAN_BLINDS_FADE
	bor ?wxAUI_MGR_RECTANGLE_HINT).
%%
mapping_info() -> [
	#gx_wx{
		type = aui, 
		attributes = record_info(fields, aui), 
		extends = gx_ui_toplevelwindow,
		wx_type = undefined,
		event_map = [{onunload, close_window}]
	}].


%% Opts = [{title, String}|{width, Integer}|{height, Integer}|{icon, Path}] etc
create(Gx, Parent, #aui{id = GxName, title = Title, icon = Icon, pos = Pos, size = Size, show = Show,
		callbacks = _Callbacks, content = Children}) ->
	Ref = gx_wx:call(Gx, ?wxFrame_new_4, [Parent, -1, Title, {options, [pos, size, style], [{pos, Pos}]}]),
	% AUI
	Manager = gx_wx:call(Gx, ?wxAuiManager_new, 
		[{options, [managed_wnd, flags], [{managed_wnd, Ref}, {flags, ?AUI_FLAGS}]}]),
	%?TTY({create, Name, Ref}),
	config(Gx, Ref, icon, Icon),
	% NOTE: anonymous event for detecting frame closes where no callback is specified
	ok = gx_ui:bind_event(Gx, Ref, {close_window, {GxName, local}}),
	
	Sizer = gx_ui_sizer:create(Gx, Ref, vertical, Size),
	Panes = gx_ui:create(Gx, Ref, Children),
	?TTY({frame_kids, Panes}),
	[add_pane(Gx, Manager, KRef) || #gx_ui{ref = KRef} <- Panes],
	gx_ui_toplevelwindow:layout(Gx, Parent, Ref, Sizer),
	
	gx_wx:cast(Gx, ?wxAuiManager_Update, [Manager]),
	config(Gx, Ref, show, Show),
	#gx_ui{id = GxName, ref = Ref, parent = Parent}.

read(Gx, Ref, Key) ->
	gx_ui_toplevelwindow:read(Gx, Ref, Key).

config(Gx, Ref, K, V) ->
	gx_ui_toplevelwindow:config(Gx, Ref, K, V).

add_pane(Gx, AuiManager, Ref) ->
	true = gx_wx:call(Gx, ?wxAuiManager_AddPane_2_0, [AuiManager, Ref, {options, [direction, caption], []}]).
	
