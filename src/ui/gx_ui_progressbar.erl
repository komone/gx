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

-module(gx_ui_progressbar).

-include("gx.hrl").
-include_lib("gx_wx.hrl").

-behaviour(gx_ui).
-export([mapping_info/0, create/3, read/3, config/4]).

%%
mapping_info() -> [
	#gx_wx{
		type = progressbar, 
		attributes = record_info(fields, progressbar), 
		extends = gx_ui_control,
		wx_type = wxGauge,
		event_map = []
	}].

%%
create(G, Parent = #wx_ref{}, P = #progressbar{id = GxName, percent = Percent, layout = Layout, smooth = Smooth}) ->
	Style = get_style(Layout, Smooth),
	Ref = gx_wx:call(G, ?wxGauge_new_4, [Parent, -1, 100, 
		{options, [pos, size, style, validator], [{style, Style}]}]),
	case is_percent(Percent) of 
	true ->
		config(G, Ref, value, Percent);
	false -> 
		config(G, Ref, pulse, true)
	end,
	ok = gx_ui_control:init(G, Parent, P#progressbar{ref = Ref}, mapping_info()),
	#gx_ui{id = GxName, ref = Ref, parent = Parent}.
	
read(Gx, Ref, Key) ->
	gx_ui_control:read(Gx, Ref, Key).
	
config(G, Ref, pulse, true) ->
	gx_wx:cast(G, ?wxGauge_Pulse, [Ref]);
config(G, Ref, value, X) ->
	case is_percent(X) of 
	true ->
		gx_wx:cast(G, ?wxGauge_SetValue, [Ref, X]);
	false ->
		gx_wx:cast(G, ?wxGauge_Pulse, [Ref])
	end;
config(G, Ref, Property, Value) ->
	gx_ui_control:config(G, Ref, Property, Value).

	
get_style(vertical, false) ->
	?wxGA_VERTICAL;
get_style(vertical, true) ->
	?wxGA_VERTICAL bor ?wxGA_SMOOTH;
get_style(_, true) ->
	?wxGA_HORIZONTAL bor ?wxGA_SMOOTH;
get_style(_, _) ->
	?wxGA_HORIZONTAL.

is_percent(Percent) ->
	is_integer(Percent) andalso Percent >= 0 andalso Percent =< 100.
