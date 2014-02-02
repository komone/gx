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

-module(gx_ui_frame).

-include("gx.hrl").
-include("gx_wx.hrl").

-behaviour(gx_ui).
-export([mapping_info/0, create/3, read/3, config/4]).

%%
mapping_info() -> [
	#gx_wx{
		type = frame, 
		attributes = record_info(fields, frame), 
		extends = gx_ui_toplevelwindow,
		wx_type = wxFrame,
		event_map = [{onunload, close_window}]
	}].


%% Opts = [{title, String}|{width, Integer}|{height, Integer}|{icon, Path}] etc
create(Gx, Parent, #frame{
		id = GxName, title = Title, icon = Icon, 
		pos = Pos, size = Size, show = Show, virtual = Virtual,
		callbacks = _Callbacks, content = Children}) ->
	Centered = Pos =:= center,
	Pos0 =
		case Centered of
		true ->
			{-1, -1};
		_ ->
			Pos
		end,
	case Virtual of
	false ->
		Ref = gx_wx:call(Gx, ?wxFrame_new_4, [Parent, -1, Title, {options, [pos, size, style], [{pos, Pos0}]}]);
	true ->
		StyleFlags = ?wxCAPTION bor ?wxSYSTEM_MENU bor ?wxCLOSE_BOX,
		Ref = wxMiniFrame:new(Parent, -1, binary_to_list(Title), [{style, StyleFlags}])
	end,
%	Ref = #wx_ref{} = create_frame(Gx, Parent, Title, [{pos, Pos0}], Virtual),
	%?TTY({create, Name, Ref}),
	config(Gx, Ref, icon, Icon),
	% NOTE: anonymous event for detecting frame closes where no callback is specified
	ok = gx_ui:bind_event(Gx, Ref, {close_window, {GxName, local}}),
	
	Sizer = gx_ui_sizer:create(Gx, Ref, vertical, {-1, -1}),
	_Components = gx_ui:create(Gx, Ref, Children),

	gx_ui_toplevelwindow:layout(Gx, Parent, Ref, Sizer),
	case Size of 
	{-1, -1} ->
		ok;
	{_, _} ->
		config(Gx, Ref, size, Size);
	_ ->
		ok
	end,
	case Centered of
	true ->
		config(Gx, Ref, center, true);
	false ->
		ok
	end,
	config(Gx, Ref, show, Show),
	#gx_ui{id = GxName, ref = Ref, parent = Parent}.

read(Gx, Ref, Key) ->
	gx_ui_toplevelwindow:read(Gx, Ref, Key).

config(Gx, Ref, K, V) ->
	gx_ui_toplevelwindow:config(Gx, Ref, K, V).

