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

-module(gx_ui_control).

-include("gx.hrl").
-include("gx_wx.hrl").

-export([init/4, read/3, config/4]).

%% Generic control
init(Gx, Parent,
		#control{id = Name, ref = Ref, size = {Width, Height}, align = Align, 
			border = Border, fill = Fill, enable = Enable, callbacks = Callbacks}, EventMap) when is_list(EventMap) -> 
	%% set window name
	config(Gx, Ref, name, Name),
	config(Gx, Ref, enable, Enable),
	% NOTE: set min size on Sizer instead?
	gx_wx:cast(Gx, ?wxWindow_SetMinSize, [Ref, Width, Height]),	
	ParentSizer = gx_wx:call(Gx, ?wxWindow_GetSizer, [Parent]),
	SizerOptions = gx_ui_sizer:get_sizer_flags(Gx, ParentSizer, Align, Fill, Border),
%	?TTY({sizer_options, {Orientation, Align, Fill, Border}, SizerOptions}),
	gx_ui_sizer:add(Gx, ParentSizer, Ref, SizerOptions),	
	gx_ui:bind_events(Gx, Ref, Name, Callbacks, EventMap);
%
init(Gx, Parent, Control, [#gx_wx{event_map = EventMap}|_]) when is_tuple(Control), tuple_size(Control) >= 9 -> 
	Record = #control{
		id = element(2, Control),
		ref = element(3, Control),
		label = element(4, Control),
		size = element(5, Control),
		align = element(6, Control),
		border = element(7, Control),
		fill = element(8, Control),
		enable = element(9, Control),
		callbacks = element(10, Control)
	},
	init(Gx, Parent, Record, EventMap).

%%
read(Gx, Ref, label) ->
	String = gx_wx:call(Gx, ?wxControl_GetLabel, [Ref]),
	list_to_binary(String);
read(Gx, Ref, Key) ->
	gx_ui_window:read(Gx, Ref, Key).
	
%%
config(Gx, Ref, label, Value) ->
	gx_wx:cast(Gx, ?wxControl_SetLabel, [Ref, Value]);
config(Gx, Ref, Key, Value) ->
	gx_ui_window:config(Gx, Ref, Key, Value).
