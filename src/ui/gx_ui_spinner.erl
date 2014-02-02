%% Copyright 2010 Steve Davis <steve@simulacity.com>
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

-module(gx_ui_spinner).

-include("gx.hrl").
-include("gx_wx.hrl").

-behaviour(gx_ui).
-export([mapping_info/0, create/3, read/3, config/4]).

%%
mapping_info() -> [
	#gx_wx{
		type = spinner, 
		attributes = record_info(fields, spinner), 
		extends = gx_ui_control,
		wx_type = wxSpinCtrl,
		event_map = [{onchange, command_spinctrl_updated}]
	}].

%%
create(Gx, Parent = #wx_ref{}, S = #spinner{id = GxName, min = Min, max = Max, value = Value, wrap = Wrap}) ->
	Opts = [{style, get_style(Wrap)}, {min, Min}, {max, Max}, {initial, Value}],
	Ref = gx_wx:call(Gx, ?wxSpinCtrl_new_2, [Parent, {options, [id, value, pos, size, style, min, max, initial], Opts}]),
	ok = gx_ui_control:init(Gx, Parent, S#spinner{ref = Ref}, mapping_info()),
	#gx_ui{id = GxName, ref = Ref, parent = Parent}.
%
get_style(true) ->
	?wxSP_WRAP;
get_style(_) ->
	0.

%%
read(G, Ref, Key) ->
	gx_ui_control:read(G, Ref, Key).

%%
config(G, Ref, Key, Value) ->
	gx_ui_control:config(G, Ref, Key, Value).
