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

-module(gx_ui_grid).

-include("gx.hrl").
-include_lib("gx_wx.hrl").

-behaviour(gx_ui).
-export([mapping_info/0, create/3, read/3, config/4]).


%%
mapping_info() -> [
	#gx_wx{
		type = grid, 
		attributes = record_info(fields, grid), 
		extends = gx_ui_control,
		wx_type = wxGrid,
		event_map = []
	}].

create(Gx, Parent, #grid{id = GxName, size = Size, cols = Cols, rows = Rows, align = Align, fill = Fill, border = Border}) ->
	Ref = gx_wx:call(Gx, ?wxGrid_new_3, [Parent, ?wxID_ANY, {options, [pos, size, style], []}]),
	gx_wx:call(Gx, ?wxGrid_CreateGrid, [Ref, Rows, Cols, {options, [selmode], []}]),
	gx_wx:cast(Gx, ?wxGrid_SetColLabelSize, [Ref, 0]),
	gx_wx:cast(Gx, ?wxGrid_SetRowLabelSize, [Ref, 0]),
	gx_wx:cast(Gx, ?wxGrid_SetMargins, [Ref, 0, 0]),
	
	Sizer = gx_ui_sizer:create(Gx, Ref, vertical, Size),
%	gx_ui:create(Gx, Ref, Children),
	_NewSize = gx_ui_sizer:layout(Gx, Parent, Ref, Sizer, Align, Fill, Border),	
	#gx_ui{id = GxName, ref = Ref, parent = Parent}.

read(Gx, Ref, Key) -> 
	gx_ui_panel:read(Gx, Ref, Key). %% NOTE: gx_ui_scrolled_window...

config(Gx, Ref, Key, Value) -> 
	gx_ui_panel:config(Gx, Ref, Key, Value). %% NOTE: gx_ui_scrolled_window...
