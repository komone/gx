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

-module(gx_ui_panel).

-include("gx.hrl").
-include("gx_wx.hrl").

-behaviour(gx_ui).
-export([mapping_info/0, create/3, read/3, config/4]).

%%
mapping_info() -> [
	#gx_wx{
		type = panel, 
		attributes = record_info(fields, panel), 
		extends = gx_ui_window,
		wx_type = wxPanel,
		event_map = []
	},
	#gx_wx{
		type = box, 
		attributes = record_info(fields, box), 
		extends = gx_ui_window,
		wx_type = wxPanel,
		event_map = []
	}].

%%
create(Gx, Parent, #panel{id = GxName, size = Size, background = Background,
		align = Align, border = Border, fill = Fill,
		color = Color, layout = Layout, cols = Cols, rows = Rows, content = Children}) ->
	Ref = init_panel(Gx, Parent, GxName, Color, Background),
	Sizer = gx_ui_sizer:create(Gx, Ref, Layout, Size, {Cols, Rows}),	
	gx_ui:create(Gx, Ref, Children),
	_NewSize = gx_ui_sizer:layout(Gx, Parent, Ref, Sizer, Align, Fill, Border),
	%?TTY({fit, Ref, NewSize}),
	#gx_ui{id = GxName, ref = Ref, parent = Parent};

%%
create(Gx, Parent, #box{id = GxName, color = Color,
		label = Label, size = Size, align = Align, border = Border, fill = Fill, 
		layout = Layout, content = Children}) ->
	Ref = init_panel(Gx, Parent, GxName, Color, undefined),
	Sizer = gx_ui_sizer:create_box(Gx, Ref, Layout, Size, Label),
	gx_ui:create(Gx, Ref, Children),
	_NewSize = gx_ui_sizer:layout(Gx, Parent, Ref, Sizer, Align, Fill, Border),
	%?TTY({fit, Ref, NewSize}),
	#gx_ui{id = GxName, ref = Ref, parent = Parent}.

%%
read(G, Ref, Key) ->
	gx_ui_window:read(G, Ref, Key).

%%
config(G, Ref, show, Value) ->
	gx_ui_sizer:show(G, Ref, Value);
config(G, Ref, Key, Value) ->
	gx_ui_window:config(G, Ref, Key, Value).

%
init_panel(Gx, Parent, GxName, Color, Background) ->
	Ref = gx_wx:call(Gx, ?wxPanel_new_2, [Parent, {options, [winid, pos, size, style], []}]),
	gx_wx:cast(Gx, ?wxWindow_SetName, [Ref, atom_to_binary(GxName, utf8)]),
	set_color(Gx, Ref, Color),
	set_background(Gx, Ref, Background),
	Ref.

%	
set_color(_Gx, _Panel, undefined) ->
	ok;
set_color(Gx, Panel, ColorName) when is_atom(ColorName) ->
	ColorValue = gx_color:decode(ColorName),
	set_color(Gx, Panel, ColorValue);
set_color(Gx, Panel, {R, G, B}) ->
	set_color(Gx, Panel, {R, G, B, 255});
set_color(Gx, Panel, Color = {_, _, _, _}) ->
	gx_wx:call(Gx, ?wxWindow_SetBackgroundColour, [Panel, Color]).

%
set_background(_Gx, Panel, Background) when is_binary(Background) ->
	Path = path:new(Background),
	case path:type(Path) of
	regular ->				
		F = fun(Evt = #wxErase{type = _Type}) ->
				?TTY({setting_background, Evt, Path}),
				Image = wxBitmap:new(binary_to_list(Path), [{type, ?wxBITMAP_TYPE_PNG}]),
				CDC = wxClientDC:new(Panel),
				ok = wxDC:clear(CDC),
				wxDC:drawBitmap(CDC, Image, {5, 5}, [{useMask, false}]),
				wxClientDC:destroy(CDC),
				?TTY({disconnect, self()})
				%wxEvtHandler:disconnect(Panel, Type)
			end,
		?TTY({connect, self()}),
		wxEvtHandler:connect(Panel, erase_background, [{userData, F}]);
	_ ->
		?TTY({invalid_file, Path})
	end;
set_background(_, _, _) ->
	ok.
