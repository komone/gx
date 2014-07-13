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

-module(gx_ui_tree).

-include("gx.hrl").
-include("gx_wx.hrl").

-behaviour(gx_ui).
-export([mapping_info/0, create/3, read/3, config/4]).

%%
mapping_info() -> [
	#gx_wx{
		type = tree, 
		attributes = record_info(fields, tree), 
		extends = gx_ui_control,
		wx_type = wxTreeCtrl,
		event_map = [
			{onchange, command_tree_item_collapsing},
			{onchange, command_tree_item_expanding},
			{onclick, command_tree_sel_changed},
			{ondblclick, command_tree_item_activated}
		]
	}].

%%
create(Gx, Parent, T = #tree{id = GxName, label = Label, icon = Icon, content = Children, data = Data}) ->
	Ref = gx_wx:call(Gx, ?wxTreeCtrl_new_2, [Parent, {options, [id, pos, size, style, validator], 
		[{style, ?wxTR_HIDE_ROOT bor ?wxTR_HAS_BUTTONS bor ?wxTR_LINES_AT_ROOT}]}]),
	
	{ok, ImageList} = gx_cache:get_image_list(Gx),
	gx_wx:cast(Gx, ?wxTreeCtrl_SetImageList, [Ref, ImageList]),
	
	case gx_cache:get_image_index(Gx, Icon) of
	{ok, Index} when is_integer(Index) ->
		Opts = [{image, Index}];
	undefined ->
		Opts = []
	end,
	% NOTE!! ({data, Data}, Acc) ->   wxe_util:send_bin(term_to_binary(Data)),[<<3:32/?UI,0:32>>|Acc];
	RootId = gx_wx:call(Gx, ?wxTreeCtrl_AddRoot, [Ref, Label, {options, [image, selectedImage, data], Opts}]),
	gx_cache:register(Gx, RootId, {root, Data}, Ref),
  
	create_children(Gx, Ref, RootId, Children, []),
	ok = gx_ui_control:init(Gx, Parent, T#tree{ref = Ref}, mapping_info()),
	%gx_wx:cast(Gx, ?wxTreeCtrl_Expand, [Ref, 0, {long, RootId}]), % <<ThisRef:32/?UI,0:32,Item:64/?UI>>).
	#gx_ui{id = GxName, ref = Ref, parent = Parent}.

%%
config(G, Ref, Key, Value) ->
	gx_ui_control:config(G, Ref, Key, Value).
	
%%
read(G, Ref, Key) ->
	gx_ui_control:read(G, Ref, Key).

%
create_children(Gx, Parent, TargetId, [I = #treeitem{label = Label, icon = Icon, data = Data, content = Children}|T], Acc) ->
	case gx_cache:get_image_index(Gx, Icon) of
	{ok, Index} when is_integer(Index) -> 
		Opts = [{image, Index}];
	undefined ->
		Opts = []
	end,
	Args = [Parent, 0, {long, TargetId}, Label, {options, [image, selectedImage, data], Opts}],
	ItemId = gx_wx:call(Gx, ?wxTreeCtrl_AppendItem, Args), 
	gx_cache:register(Gx, ItemId, {TargetId, Data}, Parent),
	Kids = create_children(Gx, Parent, ItemId, Children, []),	
	create_children(Gx, Parent, TargetId, T, [I#treeitem{ref = ItemId, content = Kids}|Acc]);
create_children(_, _Control, _Target, [], Acc) ->
	lists:reverse(Acc).

