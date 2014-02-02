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

-module(gx_ui_separator).

-include("gx.hrl").
-include("gx_wx.hrl").

-behaviour(gx_ui).
-export([mapping_info/0, create/3, read/3, config/4]).

%%
mapping_info() -> [
	#gx_wx{
		type = separator, 
		attributes = record_info(fields, separator), 
		extends = undefined,
		wx_type = undefined,
		event_map = []
	}].

%%
create(Gx, Parent = #wx_ref{type = wxMenu}, #separator{}) ->
	Ref = gx_wx:call(Gx, ?wxMenu_AppendSeparator, [Parent]),
	#gx_ui{ref = Ref, parent = Parent};
create(Gx, Parent = #wx_ref{type=wxToolBar}, #separator{}) ->
	Ref = gx_wx:call(Gx, ?wxToolBar_AddSeparator, [Parent]),
	#gx_ui{ref = Ref, parent = Parent}.

%%
read(_, _, Key) ->
	{not_found, Key}.

config(_Gx, _Ref, Key, _Value) ->
	{not_found, Key}.
