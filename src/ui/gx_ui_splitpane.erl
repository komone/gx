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

-module(gx_ui_splitpane).

-include("gx.hrl").
-include("gx_wx.hrl").

-behaviour(gx_ui).
-export([mapping_info/0, create/3, read/3, config/4]).

%%
mapping_info() -> [
	#gx_wx{
		type = splitpane,
		attributes = record_info(fields, splitpane),
		extends = gx_ui_window,
		wx_type = wxSplitterWindow,
		event_map = [{ondblclick, command_splitter_doubleclicked}]
	}].

%%
create(Gx, Parent, #splitpane{id = GxName, size = Size, align = Align, fill = Fill, border = Border, 
		layout = Layout, sash = SashPos, gravity = Gravity, content = [First, Second]}) ->
	Ref = gx_wx:call(Gx, ?wxSplitterWindow_new_2, [Parent, {options, [id, pos, size, style], [{style, ?wxSP_NOBORDER bor ?wxSP_3DSASH bor ?wxSP_LIVE_UPDATE}]}]),
	Sizer = gx_ui_sizer:create(Gx, Ref, vertical, Size),
	%% NOTE: panes are wrapped in panels to make mouse cursor work correctly
	#gx_ui{ref = FirstPaneRef} = gx_ui:create(Gx, Ref, #panel{content = [First]}),
	#gx_ui{ref = SecondPaneRef} = gx_ui:create(Gx, Ref, #panel{content = [Second]}),
	Opts = [{sashPosition, SashPos}],
	case Layout of
	vertical -> 
		true = gx_wx:call(Gx, ?wxSplitterWindow_SplitVertically, [Ref, FirstPaneRef, SecondPaneRef, 
			{options, [sashPosition], Opts}]);
	horizontal ->
		true = gx_wx:call(Gx, ?wxSplitterWindow_SplitHorizontally, [Ref, FirstPaneRef, SecondPaneRef, 
			{options, [sashPosition], Opts}]);
	_ -> 
		undefined
	end,
	Value = get_gravity(Gravity),
	gx_wx:cast(Gx, ?wxSplitterWindow_SetSashGravity, [Ref, 0, Value]),
	gx_wx:cast(Gx, ?wxSplitterWindow_SetMinimumPaneSize, [Ref, 1]),
	_NewSize = gx_ui_sizer:layout(Gx, Parent, Ref, Sizer, Align, Fill, Border),
	#gx_ui{id = GxName, ref = Ref, parent = Parent}.

%%
read(G, Ref, sash) ->
	gx_wx:call(G, ?wxSplitterWindow_GetSashPosition, [Ref]);
read(G, Ref, Key) ->
	gx_ui_window:read(G, Ref, Key).

%%
config(G, Ref, sash, Pos) ->
	gx_wx:cast(G, ?wxSplitterWindow_SetSashPosition, [Ref, Pos, {options, [redraw], []}]);
config(G, Ref, Key, Value) ->
	gx_ui_window:config(G, Ref, Key, Value).

get_gravity(Num) when is_integer(Num), Num >= 0, Num =< 100 ->
	Num / 100;
get_gravity(_) ->
	0.5.

