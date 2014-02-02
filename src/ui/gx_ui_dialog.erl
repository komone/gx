%% Copyright 2010-2012 Steve Davis <steve@simulacity.com>
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

-module(gx_ui_dialog).

-include("gx.hrl").
-include("gx_wx.hrl").

-behaviour(gx_ui).
-export([mapping_info/0, create/3, read/3, config/4]).

-compile(export_all).

%%
mapping_info() -> [
	#gx_wx{
		type = dialog, 
		attributes = record_info(fields, dialog),
		extends = gx_ui_toplevelwindow,
		wx_type = wxDialog,
		wx_parent = wxTopLevelWindow,
		event_map = [{onload, show}, {onunload, close_window}]
	}].

%% TODO: Not fully implemented
create(Gx, Parent, #dialog{id = GxName, title = Title, size = Size, 
		icon = Icon, callbacks = Callbacks, content = Children, pos = Pos, show = Show}) ->
	Ref = gx_wx:call(Gx, ?wxDialog_new_4, [Parent, ?wxID_ANY, Title, {options, [pos, size, style], []}]),
	config(Gx, Ref, icon, Icon),
	
	Sizer = gx_ui_sizer:create(Gx, Ref, vertical, Size),
	gx_ui:create(Gx, Ref, Children),

	gx_ui_toplevelwindow:layout(Gx, Parent, Ref, Sizer),
	case Pos of
	center ->
		config(Gx, Ref, center, true);
	_ ->
		ok
	end,
	config(Gx, Ref, show, Show),
	
	case Parent of
	#wx_ref{ref = 0} ->
		% NOTE: anonymous event for detecting frame closes where no callback is specified
		ok = gx_ui:bind_event(Gx, Ref, {close_window, {GxName, local}}),
		gx_ui:bind_events(Gx, Ref, GxName, Callbacks, [{onload, show}]);
	_ ->
%		config(Gx, Ref, layout, true),
%		config(Gx, Ref, fit, true),
%		gx_wx:cast(Gx, ?wxWindow_Fit, [Ref]),
%		config(Gx, Ref, show, Show),
		ok
	end,
	#gx_ui{id = GxName, ref = Ref, parent = Parent}.

read(Gx, Ref, Key) ->
	gx_ui_toplevelwindow:read(Gx, Ref, Key).

config(Gx, Ref, show, modal) ->
	gx_wx:call(Gx, ?wxDialog_ShowModal, [Ref]);
config(G, Ref, show, Value) when Value =:= true; Value =:= false ->
	gx_wx:call(G, ?wxDialog_Show, [Ref, {options, [show], [{show, Value}]}]);
config(Gx, Ref, Key, Value) ->
	gx_ui_toplevelwindow:config(Gx, Ref, Key, Value).


%% TODO: generalize these remove from registry on close!
%% Ideal would be to create these lazily but keep the reference around
%% after first use
message(Gx, Parent, Opts) ->
	Caption = proplists:get_value(title, Opts, <<"Untitled">>),
	Message = proplists:get_value(message, Opts, <<"<no message>">>),
	Style = ?wxOK bor ?wxICON_INFORMATION,
	Ref = gx_wx:call(Gx, ?wxMessageDialog_new, [Parent, Message, {options, [caption, style, pos], [{caption, Caption}, {style, Style}]}]),
	gx_ui_dialog:config(Gx, Ref, center, true),
	gx_ui_dialog:config(Gx, Ref, show, modal),
	gx_wx:destroy(Gx, Ref),
	ok.

text_entry(Gx, Parent, Opts) ->
	Caption = proplists:get_value(caption, Opts, <<"Untitled">>),
	Value = proplists:get_value(value, Opts, <<"">>),
	Style = proplists:get_value(value, Opts, {style, ?wxOK bor ?wxCANCEL}),
	Ref = gx_wx:call(Gx, ?wxTextEntryDialog_new, [Parent, 
		{options, [caption, value, style, pos], [{caption, Caption}, {value, Value}, {style, Style}] }]),
	gx_ui_dialog:config(Gx, Ref, center, true),
	gx_ui_dialog:config(Gx, Ref, show, modal),
	Result = gx_wx:call(?wxTextEntryDialog_GetValue, [Ref]),
	gx_wx:destroy(Gx, Ref),
	list_to_binary(Result).

%%
color_chooser(Gx, Parent = #wx_ref{}, _Opts) ->
	ColorRef = gx_wx:call(Gx, ?wxColourData_new_0, []),
	gx_wx:cast(Gx, ?wxColourData_SetColour, [ColorRef, {255,255,255,255}]),
	Ref = gx_wx:call(Gx, ?wxColourDialog_new_2, [Parent, {options, [data], [{data, ColorRef}]}]),
	gx_ui_dialog:config(Gx, Ref, center, true),
	gx_ui_dialog:config(Gx, Ref, show, modal),
	Color =
		case gx_wx:call(Gx, ?wxColourDialog_GetColourData, [Ref]) of
		[] ->
			undefined;
		ColorData ->
			gx_wx:call(Gx, ?wxColourData_GetColour, [ColorData])
		end,
	gx_wx:destroy(Gx, Ref),
	Color.

directory_chooser(Gx, Parent, []) ->
	directory_chooser(Gx, Parent, [{defaultPath, <<"">>}]);
directory_chooser(Gx, Parent = #wx_ref{}, Opts) ->
	Ref = gx_wx:call(Gx, ?wxDirDialog_new, [Parent, {options, [title, defaultPath, style, pos, sz], Opts}]),
	gx_ui_dialog:config(Gx, Ref, center, true),
	gx_ui_dialog:config(Gx, Ref, show, modal),
	Path = 
		case gx_wx:call(Gx, ?wxDirDialog_GetPath, [Ref]) of
		[] ->
			undefined;
		Value ->
			path:new(Value)
		end,
	gx_wx:destroy(Gx, Ref),
	Path.

%%
file_chooser(Gx, Parent = #wx_ref{}, Opts) ->
	Ref = gx_wx:call(Gx, ?wxFileDialog_new, [Parent, {options, [message, defaultDir, defaultFile, wildCard, style, pos, sz], Opts}]),
	gx_ui_dialog:config(Gx, Ref, center, true),
	gx_ui_dialog:config(Gx, Ref, show, modal),
	case gx_wx:call(Gx, ?wxFileDialog_GetPath, [Ref]) of
	[] ->
		Filename = undefined;
	Value ->
		Filename = path:new(Value)
	end,
	gx_wx:destroy(Gx, Ref),
	Filename.

%%
font_chooser(Gx, Parent = #wx_ref{}, _Opts) ->
	InitialDataRef = gx_wx:call(Gx, ?wxFontData_new_0, []),
	Ref = gx_wx:call(Gx, ?wxFontDialog_new_2, [Parent, InitialDataRef]),
	gx_ui_dialog:config(Gx, Ref, center, true),
	gx_ui_dialog:config(Gx, Ref, show, modal),
	Result = 
		case gx_wx:call(Gx, ?wxFontDialog_GetFontData, [Ref]) of
		FontData = #wx_ref{type = wxFontData} ->
			Font = #wx_ref{type = wxFont} = gx_wx:call(Gx, ?wxFontData_GetChosenFont, [FontData]),
			gx_font:decode(Gx, Font);
		_ ->
			undefined
		end,
	gx_wx:destroy(Gx, Ref),
	Result.
	
%% Actually this ia a type of wxFrame but it makes more sense here 	
splashscreen(Parent, Opts) ->
	Timeout = gx:get_integer(timeout, 5000, Opts),
	Image = gx:get_resource(image, Opts),
	wxSplashScreen:new(Image, 
		?wxSPLASH_CENTRE_ON_SCREEN bor ?wxSPLASH_TIMEOUT, 
		Timeout, Parent, -1).

