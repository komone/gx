%% Copyright 2010-2014 Steve Davis <steve@simulacity.com>
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

-module(gx_font).

-include("gx.hrl").
-include("gx_wx.hrl").

-export([decode/2]).


decode(G, Ref = #wx_ref{type = wxFont}) ->
	#font{
		id = undefined,
		family = family(gx_wx:call(G, ?wxFont_GetFamily, [Ref])),
		face = list_to_binary(gx_wx:call(G, ?wxFont_GetFaceName, [Ref])),
		size = gx_wx:call(G, ?wxFont_GetPointSize, [Ref]),
		weight = weight(gx_wx:call(G, ?wxFont_GetWeight, [Ref])),
		style = style(gx_wx:call(G, ?wxFont_GetStyle, [Ref])),
		fixed = gx_wx:call(G, ?wxFont_IsFixedWidth, [Ref])
	}.

family(?wxFONTFAMILY_DEFAULT) ->
	default;
family(?wxFONTFAMILY_DECORATIVE) ->
	decorative;
family(?wxFONTFAMILY_ROMAN) ->
	roman;
family(?wxFONTFAMILY_SCRIPT) ->
	script;
family(?wxFONTFAMILY_SWISS) ->
	swiss;
family(?wxFONTFAMILY_MODERN) ->
	modern;
family(?wxFONTFAMILY_TELETYPE) ->
	teletype.

style(?wxFONTSTYLE_NORMAL) ->
	normal;
style(?wxFONTSTYLE_ITALIC) ->
	italic;
style(?wxFONTSTYLE_SLANT) ->
	slant.

weight(?wxFONTWEIGHT_NORMAL) ->
	normal;
weight(?wxFONTWEIGHT_LIGHT) ->
	light;
weight(?wxFONTWEIGHT_BOLD) ->
	bold.
