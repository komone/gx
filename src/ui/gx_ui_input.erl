%% Copyright 2010 Steve Davis <steve@simulacity.com>
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%% 
%% http://www.apache.org/licenses/LICENSE-2.0
%% 
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(gx_ui_input).

-include("gx.hrl").
-include("gx_wx.hrl").

-behaviour(gx_ui).
-export([mapping_info/0, create/3, read/3, config/4]).

%%
mapping_info() -> [
	#gx_wx{
		type = input, 
		attributes = record_info(fields, input), 
		extends = gx_ui_control,
		wx_type = wxTextCtrl,
		event_map = [
			{onchange, command_text_updated},
		%?	{onselect, command_text_enter},
			{onsubmit, command_text_enter}
		]
	}].

%%
create(Gx, Parent, I = #input{id = GxName, style = Style, enabled = Enabled, content = Text, src = File}) ->
	%?TTY(I),
	WxStyle = get_style(Style),
	Ref = gx_wx:call(Gx, ?wxTextCtrl_new_3, [Parent, -1, 
		{options, [value, pos, size, style, validator], [{value, Text}, {style, WxStyle}]}]),
	case is_binary(File) of 
	true -> 
		case path:is_regular(File) of
		true ->
			wxTextCtrl:loadFile(Ref, binary_to_list(File));
		false ->
			ignore %% LOG!
		end;
	_ ->
		ignore
	end,
	gx_wx:cast(Gx, ?wxTextCtrl_SetEditable, [Ref, Enabled]),
	ok = gx_ui_control:init(Gx, Parent, I#input{ref = Ref}, mapping_info()),
	#gx_ui{id = GxName, ref = Ref, parent = Parent}.

%%
read(G, Ref, Key) ->
	gx_ui_control:read(G, Ref, Key).

%%
config(G, Ref, append, Text) when is_binary(Text) ->
	ok = gx_wx:cast(G, ?wxTextCtrl_AppendText, [Ref, Text]);
config(G, Ref, Key, Value) ->
	gx_ui_control:config(G, Ref, Key, Value).

get_style(multi) ->
	?wxTE_PROCESS_ENTER bor ?wxTE_PROCESS_TAB bor ?wxTE_MULTILINE;
get_style(password) ->
	?wxTE_PROCESS_ENTER bor ?wxTE_PASSWORD;
get_style(_) ->
	?wxTE_PROCESS_ENTER.
