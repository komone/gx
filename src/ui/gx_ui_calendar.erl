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

-module(gx_ui_calendar).

-include("gx.hrl").
-include("gx_wx.hrl").

-behaviour(gx_ui).
-export([mapping_info/0, create/3, read/3, config/4]).

%%
mapping_info() -> [
	#gx_wx{
		type = calendar, 
		attributes = record_info(fields, calendar), 
		extends = gx_ui_control,
		wx_type = wxCalendarCtrl,
		event_map = [
			{onselect, calendar_sel_changed}, 
			{onchange, calendar_day_changed}, 
			{onchange, calendar_month_changed}, 
			{onchange, calendar_year_changed}, 
			{ondblclick, calendar_doubleclicked},
			{onclick, calendar_weekday_clicked}
		]
	}].

%%
create(Gx, Parent, C = #calendar{id = GxName}) ->
	Ref = gx_wx:call(Gx, ?wxCalendarCtrl_new_3, [Parent, -1, {options, [date, pos, size, style], []}]),
	ok = gx_ui_control:init(Gx, Parent, C#calendar{ref = Ref}, mapping_info()),
	#gx_ui{id = GxName, ref = Ref, parent = Parent}.

read(Gx, Ref, value) ->
	gx_wx:call(Gx, ?wxCalendarCtrl_GetDate, [Ref]);
read(Gx, Ref, Key) ->
	gx_ui_control:read(Gx, Ref, Key).

config(Gx, Ref, Key, Value) ->
	gx_ui_control:config(Gx, Ref, Key, Value).

