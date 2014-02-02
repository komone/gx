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

-module(gx_ui_toplevelwindow).

-include("gx.hrl").
-include("gx_wx.hrl").

-export([read/3, config/4]).
-export([layout/4]). %% Temp?


%%
read(Gx, Ref, icon) ->
	#wx_ref{} = gx_wx:call(Gx, ?wxTopLevelWindow_GetIcon, [Ref]);
read(Gx, Ref, title) ->
	String = gx_wx:call(Gx, ?wxTopLevelWindow_GetTitle, [Ref]),
	list_to_binary(String);	
read(G, Ref, Key) ->
	gx_ui_window:read(G, Ref, Key).% delegate to superclass

%%
config(G, Ref, icon, Icon) when is_binary(Icon) ->
	set_icon(G, Ref, Icon);
config(G, Ref, K, V) ->
	gx_ui_window:config(G, Ref, K, V).

%
set_icon(_, _, undefined) ->
	ok;
set_icon(Gx, Ref, Icon) ->
	case gx_cache:get_icon(Gx, Icon) of
	{ok, WxIcon = #wx_ref{type = wxIcon}} ->
		gx_wx:cast(Gx, ?wxTopLevelWindow_SetIcon, [Ref, WxIcon]);
	_ ->
		ignore
	end.
	
% Top-Level container
layout(Gx, #wx_ref{ref = _}, Ref, Sizer) ->
	%% NOTE: size hints only "work" on a top level window
	gx_wx:cast(Gx, ?wxSizer_SetSizeHints, [Sizer, Ref]),
	gx_wx:call(Gx, ?wxWindow_Layout, [Ref]),
	gx_wx:cast(Gx, ?wxWindow_Fit, [Ref]).
