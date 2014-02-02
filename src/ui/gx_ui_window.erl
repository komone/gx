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

-module(gx_ui_window).

-include("gx.hrl").
-include("gx_wx.hrl").

-export([read/3, config/4]).

read(G, Ref, pos) ->
	{_, _} = gx_wx:call(G, ?wxWindow_GetPosition, [Ref]);
read(G, Ref, size) ->
	{_, _} = gx_wx:call(G, ?wxWindow_GetSize, [Ref]);
read(G, Ref, children) ->
	gx_wx:call(G, ?wxWindow_GetChildren, [Ref]);
read(G, Ref, fg_color) ->
	gx_wx:call(G, ?wxWindow_GetForegroundColour, [Ref]);
read(G, Ref, bg_color) ->
	gx_wx:call(G, ?wxWindow_GetBackgroundColour, [Ref]);
read(_, _, _) ->
	{error, not_supported}.

config(G, Ref, name, Name) when is_atom(Name) ->
	ok = gx_wx:cast(G, ?wxWindow_SetName, [Ref, atom_to_binary(Name, utf8)]);
config(G, Ref, size, {Width, Height}) ->
	gx_wx:cast(G, ?wxWindow_SetSize_2_0, [Ref, Width, Height]);
config(G, Ref, center, true) ->
	do_center(G, Ref, ?wxBOTH);
config(G, Ref, center, horizontal) ->
	do_center(G, Ref, ?wxHORIZONTAL);
config(G, Ref, center, vertical) ->
	do_center(G, Ref, ?wxVERTICAL);
config(Gx, Ref, clear, true) ->
	gx_wx:call(Gx, ?wxWindow_DestroyChildren, [Ref]);
config(Gx, Ref, enable, Value) ->
	gx_wx:call(Gx, ?wxWindow_Enable, [Ref, {options, [enable], [{enable, Value}]}]);
config(G, Ref, layout, true) ->
%	Sizer = gx_wx:call(G, ?wxWindow_GetSizer, [Ref]),
%	gx_wx:call(G, ?wxSizer_Fit, [Sizer, Ref]),
	gx_wx:cast(G, ?wxWindow_Layout, [Ref]);
config(G, Ref, fit, true) ->
	gx_wx:cast(G, ?wxWindow_Fit, [Ref]);
config(G, Ref, fitinside, true) ->
	gx_wx:cast(G, ?wxWindow_FitInside, [Ref]);
config(G, Ref, refresh, true) ->
	gx_wx:cast(G, ?wxWindow_Refresh, [Ref, {options, [eraseBackground, rect], []}]);
config(G, Ref, update, true) ->
	gx_wx:cast(G, ?wxWindow_Update, [Ref]);
config(G, Ref, label, Value) when is_binary(Value) ->
	gx_wx:cast(G, ?wxWindow_SetLabel, [Ref, Value]);
config(G, Ref, show, Value) when Value =:= true; Value =:= false ->
	gx_wx:call(G, ?wxWindow_Show, [Ref, {options, [show], [{show, Value}]}]);
config(G, Ref, fg_color, Value) ->
	Bin = gx_color:encode(Value),
	gx_wx:cast(G, ?wxWindow_SetForegroundColour, [Ref, Bin]);
config(G, Ref, bg_color, Value) ->
	Bin = gx_color:encode(Value),
	gx_wx:cast(G, ?wxWindow_SetBackgroundColour, [Ref, Bin]);
config(G, Ref, fontsize, Value) ->
	FontRef = get_font(G, Ref),
	gx_wx:cast(G, ?wxFont_SetPointSize, [FontRef, Value]),
	gx_wx:cast(G, ?wxWindow_SetFont, [Ref, FontRef]);
config(_G, Ref, K, V) -> 
	% delegate to evt handler superclass wxEventHandler
	{error, not_supported, Ref, {K, V}}.

do_center(G, Ref, Flags) ->
	gx_wx:cast(G, ?wxWindow_Centre, [Ref, {options, [dir], [{dir, Flags}]}]).

%% BUG - GetFont may randomly return "true" instead of the font reference
get_font(G, Ref) ->
	get_font(G, Ref, 3).
get_font(G, Ref, Attempts) when Attempts > 1 ->
	case gx_wx:call(G, ?wxWindow_GetFont, [Ref]) of
	FontRef = #wx_ref{type = wxFont} ->
		FontRef;
	true ->
		get_font(G, Ref, Attempts - 1)
	end;
get_font(G, Ref, _) ->
	#wx_ref{type = wxFont} = gx_wx:call(G, ?wxWindow_GetFont, [Ref]).
