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

-module(gx_ui_line).

-include("gx.hrl").
-include("gx_wx.hrl").

-behaviour(gx_ui).
-export([mapping_info/0, create/3, read/3, config/4]).

%%
mapping_info() -> [
	#gx_wx{
		type = line, 
		attributes = record_info(fields, line), 
		extends = gx_ui_control,
		wx_type = wxStaticLine,
		event_map = []
	}].

%%
create(Gx, Parent, #line{id = GxName, size = Size, orientation = Orientation}) ->
	Style = get_orientation(Orientation),
	Opts = [{size, Size}, {style, Style}],
	Ref = gx_wx:call(Gx, ?wxStaticLine_new_2, [Parent, {options, [id, pos, size, style], Opts}]),
	ok = gx_ui_control:init(Gx, Parent, #line{ref = Ref}, mapping_info()),
	#gx_ui{id = GxName, ref = Ref, parent = Parent}.

%%
read(G, Ref, Key) ->
	gx_ui_control:read(G, Ref, Key).

%%
config(G, Ref, Key, Value) ->
	gx_ui_control:config(G, Ref, Key, Value).
	
%
get_orientation(vertical) ->
	?wxLI_VERTICAL;
get_orientation(_) ->
	?wxLI_HORIZONTAL.
