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

-module(gx_ui_slider).

-include("gx.hrl").
-include("gx_wx.hrl").

-behaviour(gx_ui).
-export([mapping_info/0, create/3, read/3, config/4]).

%%
mapping_info() -> [
	#gx_wx{
		type = slider, 
		attributes = record_info(fields, slider), 
		extends = gx_ui_control,
		wx_type = wxSlider,
		event_map = [{onchange, command_slider_updated}]
	}].

%%
create(Gx, Parent, S = #slider{id = GxName, min = Min, max = Max, value = Value, ticks = Ticks, labels = Labels}) ->
	Opts = [{style, get_style(Ticks, Labels)}],
	Ref = gx_wx:call(Gx, ?wxSlider_new_6, [Parent, -1, {Value, Min, Max, 0}, {options, [pos, size, style, validator], Opts}]),
	ok = gx_ui_control:init(Gx, Parent, S#slider{ref = Ref}, mapping_info()),
	#gx_ui{id = GxName, ref = Ref, parent = Parent}.

read(Gx, Ref, Key) ->
	gx_ui_control:read(Gx, Ref, Key).
	
config(Gx, Ref, Key, Value) ->
	gx_ui_control:config(Gx, Ref, Key, Value).

%%
get_style(Ticks, Labels) ->
	?wxSL_HORIZONTAL bor ?wxSL_BOTTOM bor get_ticks(Ticks) bor get_labels(Labels).
%	
get_ticks(true) ->
	?wxSL_AUTOTICKS;
get_ticks(_) ->
	0.
%	
get_labels(true) ->
	?wxSL_LABELS;
get_labels(_) ->
	0.

  
  
  
  
