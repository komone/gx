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

-module(gx_ui_filetree).

-include("gx.hrl").
-include("gx_wx.hrl").

-behaviour(gx_ui).
-export([mapping_info/0, create/3, read/3, config/4]).

%%
mapping_info() -> [
	#gx_wx{
		type = filetree, 
		attributes = record_info(fields, filetree), 
		extends = gx_ui_tree,
		wx_type = undefined,
		event_map = []
	}].

%%
create(Gx, Parent, _F = #filetree{id = Id, fill = Fill, size = Size, path = Path, callbacks = Callbacks}) ->
	%?TTY(F),
	Path0 = path:new(Path),
	directory = path:type(Path),
	RootName = path:basename(Path0),
%	Files = path:list(Path0),
	Items = build_items(Path0, 4),
	Tree = #tree{id = Id, size = Size, fill = Fill, label = RootName, icon = <<"package.gif">>, 
			callbacks = Callbacks, content = Items, data = {directory, Path0}},
	gx_ui_tree:create(Gx, Parent, Tree).

%%
read(Gx, Ref, Key) ->
	gx_ui_tree:read(Gx, Ref, Key).

%%
config(Gx, Ref, Key, Value) ->
	gx_ui_tree:config(Gx, Ref, Key, Value).

build_items(Path, Depth) when Depth > 0 ->
	Files = path:list(Path),
	Files0 = [{path:name(File), path:type(File), File} || File <- Files],
	{Directories, RegularFiles} = lists:partition(fun({_, Type, _}) -> Type =:= directory end, Files0),
	build_items(lists:append(lists:sort(Directories), lists:sort(RegularFiles)), [], Depth - 1);
build_items(_, _) ->
	[].

build_items([{<<$., _Name/binary>>, _, _}|T], Acc, Depth) ->
	build_items(T, Acc, Depth);
build_items([{Name, Folder, Path}|T], Acc, Depth) ->
%	?TTY(E),
	Icon = get_icon(Folder, Path),
	case Folder of
	directory ->
		Content = build_items(Path, Depth),
		Item = #treeitem{label = Name, icon = Icon, data = {directory, Path}, content = Content};
	_ -> 
		Item = #treeitem{label = Name, icon = Icon, data = {regular, Path}, content = []}
	end,
	build_items(T, [Item | Acc], Depth);
build_items([], Acc, _) ->
	lists:reverse(Acc).

	
get_icon(directory, Path) ->
	case path:name(Path) of
	<<"ebin">> ->
		<<"packagefolder.gif">>;
	_ ->
		<<"folder.gif">>
	end;
get_icon(regular, Path) ->
	case path:extension(Path) of
	<<"beam">> ->
		<<"beam.gif">>;
	<<"erl">> ->
		<<"source.gif">>;
	_ ->
		<<"file.gif">>
	end.
