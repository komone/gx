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

-module(gx_ui_treebook).

-include("gx.hrl").
-include("gx_wx.hrl").

-behaviour(gx_ui).
-export([mapping_info/0, create/3, config/4, read/3]).

%%
mapping_info() -> [
	#gx_wx{
		type = treebook,
		attributes = record_info(fields, treebook), 
		extends = gx_ui_control,
		wx_type = wxTreebook,
		event_map = [
			{onchange, command_tree_item_expanding},
			{ondblclick, command_tree_item_activated}
		]
	}].

%%
create(G, Parent, T = #treebook{id = GxName, content = Children, size = Size}) -> 
	Ref = gx_wx:call(G, ?wxTreebook_new_3, [Parent, -1, {options, [pos, size, style], []}]),
	{ok, ImageList} = gx_cache:get_image_list(G),
	gx_wx:cast(G, ?wxTreebook_SetImageList, [Ref, ImageList]),
	_Sizer = gx_ui_sizer:create(G, Ref, column, Size),
	[create_page(G, Ref, Page) || Page <- Children],
	ok = gx_ui_control:init(G, Parent, T#treebook{ref = Ref}, mapping_info()),
	#gx_ui{id = GxName, ref = Ref, parent = Parent}.

%%
read(G, Ref, Key) ->
	gx_ui_control:read(G, Ref, Key).

%%
config(G, Ref, Key, Value) ->
	gx_ui_control:config(G, Ref, Key, Value).
	
%%
create_page(G, Parent, Page) ->
	#gx_ui{ref = Ref} = gx_ui:create(G, Parent, #panel{fill = both, content = [Page]}),
%	_IconRef = 
%		case gx_cache:get_image_index(G, ?BOGUS_ICON) of 
%		undefined ->
%			[];
%		{ok, Value} when is_integer(Value) ->
%			[{imageId, Value}]
%		end,
	true = gx_wx:call(G, ?wxTreebook_AddPage, [Parent, Ref, <<"[page]">>, {options, [bSelect, imageId], []}]),
	Ref.
