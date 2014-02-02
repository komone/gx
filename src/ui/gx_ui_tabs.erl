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

-module(gx_ui_tabs).

-include("gx.hrl").
-include("gx_wx.hrl").

-behaviour(gx_ui).
-export([mapping_info/0, create/3, read/3, config/4]).

%%
mapping_info() -> [
	#gx_wx{
		type = tabs, 
		attributes = record_info(fields, tabs), 
		extends = gx_ui_control,
		wx_type = wxNotebook,
		event_map = []
	}].

%%
create(Gx, Parent, #tabs{id = GxName, size = Size, align = Align, border = Border, fill = Fill, content = Children}) ->	
	%[{style, ?wxNB_LEFT}],
	Ref = gx_wx:call(Gx, ?wxNotebook_new_3, [Parent, -1, {options, [pos, size, style], []}]),
	{ok, ImageList} = gx_cache:get_image_list(Gx),
%	config(Gx, Ref, bg_color, {230, 230, 230, 255}),
	gx_wx:cast(Gx, ?wxNotebook_SetImageList, [Ref, ImageList]),	
	Sizer = gx_ui_sizer:create(Gx, Ref, vertical, Size),
	gx_ui:create(Gx, Ref, Children),
	gx_ui_sizer:layout(Gx, Parent, Ref, Sizer, Align, Fill, Border),
	#gx_ui{id = GxName, ref = Ref, parent = Parent}.
	
%%
read(G, Ref, Key) ->
	gx_ui_control:read(G, Ref, Key).

%%
config(G, Ref, label, Text) ->
	Selection = gx_wx:call(G, ?wxNotebook_GetSelection, [Ref]),
	true = is_integer(Selection),
	true = is_binary(Text),
	gx_wx:cast(G, ?wxNotebook_SetPageText, [Ref, Selection, Text]);
config(G, Ref, Key, Value) ->
	gx_ui_control:config(G, Ref, Key, Value).
