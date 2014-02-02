%% Copyright 2011-2014 Steve Davis <steve@simulacity.com>
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

-module(gx_ui).

-include("gx.hrl").
-include("gx_wx.hrl").

-export([behaviour_info/1]).

-export([create_tree/3, create/3, read/3, config/2, config/3, destroy/2]).

-export([bind_events/5, bind_event/3]).

%% If we go for dynamic ui module recognition and loading, we will have to
%% add {mapping_info, 0} to ui callbacks. see gx_ui_frame 
behaviour_info(callbacks) -> 
	[{mapping_info, 0}, {create, 3}, {read, 3}, {config, 4}].
%behaviour_info(_) ->
%    undefined. 

%%
create_tree(Gx = #g{}, Parent, Defs) ->
	gx_wx:batch(Gx, fun() -> create(Gx, Parent, Defs) end).
	
%%
create(Gx, Parent, Records) when is_list(Records) ->
	[create(Gx, Parent, X) || X <- Records];
create(Gx = #g{}, Parent, Record) when is_tuple(Record) ->
%	{ok, Module} = gx_cache:get_module(Gx, Record),
	Module = gx_cache:get_module(Gx, Record),
%	?TTY({Module, Parent, Record}),
	Component = Module:create(Gx, Parent, Record),
	{ok, _Ref} = gx_cache:register(Gx, Component),
	Component.

%%
read(Gx, Name, Key) ->
	{ok, Module, Ref} = gx_cache:lookup_module(Gx, Name),
%	?TTY({read, Name, Ref, Module}),
	Module:read(Gx, Ref, Key).

%%
config(Gx, List) ->
	gx_wx:batch(Gx, fun() -> [Module:config(Gx, Ref, K, V) || {Module, Ref, K, V} <- List] end).

config(Gx, Name, Properties) when is_list(Properties) ->
	{ok, Module, Ref} = gx_cache:lookup_module(Gx, Name),
	[Module:config(Gx, Ref, K, V) || {K, V} <- Properties];
config(Gx, Name, {K, V}) when is_atom(Name) ->
	{ok, Module, Ref} = gx_cache:lookup_module(Gx, Name),
	%?TTY({config, Name, Ref, Module}),
	Module:config(Gx, Ref, K, V).
	
%%
destroy(Gx, Ref) ->
	gx_wx:destroy(Gx, Ref).


bind_events(Gx, Ref = #wx_ref{}, GxName, Callbacks, EventMap) ->
	Events = [
		{WxEvent, {GxName, GxEvent, GxHandler}} || 
			{GxEvent, GxHandler} <- Callbacks, 
			{GxEvent1, WxEvent} <- EventMap, 
			GxEvent =:= GxEvent1 ],
	[bind_event(Gx, Ref, Event) || Event <- Events],
	ok.
	
bind_event(Gx, Ref, Event) ->
	gx_srv:bind_event(Gx, Ref, Event).


