%% Copyright 2012 Steve Davis <steve@simulacity.com>
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

-module(gx_ui_picker).

-include("gx.hrl").
-include("gx_wx.hrl").

-behaviour(gx_ui).
-export([mapping_info/0, create/3, read/3, config/4]).

%%
mapping_info() -> [
	#gx_wx{
		type = picker, 
		attributes = record_info(fields, picker), 
		extends = gx_ui_control,
		wx_type = wxPickerBase,
		event_map = [{onchange, command_dirpicker_changed}]
	}].
	
%% 
create(G, Parent, P = #picker{id = GxName, type = directory, value = Path, label = Label}) when is_binary(Label) ->
	Ref = gx_wx:call(G, ?wxDirPickerCtrl_new_3, [Parent, -1, {options, [path, message, pos, size, style, validator], 
		[{path, Path}, {message, Label}]}]),
	ok = gx_ui_control:init(G, Parent, P#picker{ref = Ref}, mapping_info()),
	#gx_ui{id = GxName, ref = Ref, parent = Parent}.


read(Gx, Ref = #wx_ref{type = wxDirPickerCtrl}, value) ->
	String = gx_wx:call(Gx, ?wxDirPickerCtrl_GetPath, [Ref]),
	list_to_binary(String);
%%
read(Gx, Ref, Key) ->
	gx_ui_control:read(Gx, Ref, Key).

%%
config(Gx, Ref, K, V) ->
	gx_ui_control:config(Gx, Ref, K, V).