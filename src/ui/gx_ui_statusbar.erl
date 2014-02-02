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

-module(gx_ui_statusbar).

-include("gx.hrl").
-include("gx_wx.hrl").

-behaviour(gx_ui).
-export([mapping_info/0, create/3, read/3, config/4]).

%%
mapping_info() -> [
	#gx_wx{
		type = statusbar, 
		attributes = record_info(fields, statusbar), 
		extends = gx_ui_control,
		wx_type = wxStatusBar,
		event_map = []
	}].

%% This control will become far more complex when fully implemented.
create(Gx, Parent = #wx_ref{type = wxFrame}, #statusbar{id = GxName, message = Text}) ->
	Ref = gx_wx:call(Gx, ?wxFrame_CreateStatusBar, [Parent, {options, [number, style, id], []}]),
%	Ref = gx_wx:call(Port, ?wxStatusBar_new_2, [Parent, {options, [winid, style], []}]),
%	?TTY({create, ID, Ref}),
%	Sizer = wxBoxSizer:new(?wxVERTICAL),
%	wxWindow:setSizer(StatusBar, Sizer),
%	wxSizer:setSizeHints(Sizer, Parent),
%	Sizer = gx_wx:call(Port, ?wxBoxSizer_new, [?wxVERTICAL]),
%	gx_wx:cast(Port, ?wxSizer_SetMinSize_1, [Sizer, Width, Height]),
%	gx_wx:cast(Port, ?wxWindow_SetSizer, [Parent, Sizer, 0]),
%	gx_wx:cast(Port, ?wxSizer_SetSizeHints, [Sizer, Parent]),
%	gx_wx:cast(Port, ?wxFrame_SetStatusBar, [Ref, Parent]),
	gx_wx:cast(Gx, ?wxStatusBar_SetStatusText, [Ref, Text, {options, [number], []}]),	
	#gx_ui{id = GxName, ref = Ref, parent = Parent}.

%%
read(G, Ref, Key) ->
	gx_ui_control:read(G, Ref, Key).

%%
config(G, Ref, Key, Value) ->
	gx_ui_control:config(G, Ref, Key, Value).
