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

-module(gx_cache).

-include("gx.hrl").
-include("gx_wx.hrl").

-export([start_link/2, stop/1]).
-export([register/2, register/4, lookup/2, get_module/2, lookup_module/2,
	names/1, types/1, paths/1, bind_command/4, get_command/2, get_type/2]).
-export([get_image_list/1, get_image_list/2, get_image_index/2, get_icon/2, get_bitmap/2, load_image/2]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, 
	handle_info/2, terminate/2, code_change/3]).

-record(state, {g, constants, types, modules, paths, names, refs, 
		ids, image_ref, image_ref32, images, command_index, commands}).

%%
start_link(G, InitialCache = #gx_cache{}) ->
	gen_server:start_link(?MODULE, [G, InitialCache], []).
%%
stop(#g{cache = Pid}) ->
    ok = gen_server:cast(Pid, stop).

%%
register(#g{cache = Pid}, Component = #gx_ui{}) ->
	gen_server:call(Pid, {register, Component}).
	
%%
register(#g{cache = Pid}, Id, Data, Parent) when is_integer(Id) -> 
	gen_server:call(Pid, {register, Id, Data, Parent}).

%%
lookup(#g{cache = Pid}, ID) when is_integer(ID) ->
	gen_server:call(Pid, {lookup_id, ID});
lookup(#g{cache = Pid}, Name) when is_atom(Name) ->
	gen_server:call(Pid, {lookup, Name});
lookup(#g{cache = Pid}, Ref = #wx_ref{}) ->
	gen_server:call(Pid, {reverse_lookup, Ref}).

%%
get_type(#g{cache = Pid}, #wx_ref{type = WxType}) ->
	gen_server:call(Pid, {lookup_type, WxType}).
	
%%
names(#g{cache = Pid}) ->
	gen_server:call(Pid, names).
%%
types(#g{cache = Pid}) ->
	gen_server:call(Pid, types).
%%
paths(#g{cache = Pid}) ->
	gen_server:call(Pid, paths).

%%
get_module(Gx, Record) when is_tuple(Record) ->
	get_module(Gx, element(1, Record));
get_module(#g{cache = Pid}, Type) when is_atom(Type) ->
	gen_server:call(Pid, {find_module, Type}).
%%
lookup_module(#g{cache = Pid}, GxName) when is_atom(GxName) ->
	gen_server:call(Pid, {lookup_module, GxName}).

%%
bind_command(#g{cache = Pid}, Source, Event, Handler) -> 
	gen_server:call(Pid, {bind_command, Source, Event, Handler}).
%%
get_command(#g{cache = Pid}, Index) when is_integer(Index) ->
	gen_server:call(Pid, {find_command, Index}).
%%
get_image_list(G) ->
	get_image_list(G, default).
%
get_image_list(#g{cache = Pid}, Name) when is_atom(Name) ->
	gen_server:call(Pid, {image_list, Name}).

%%
get_image_index(_, undefined) ->
	undefined;
get_image_index(#g{cache = Pid}, Path) when is_binary(Path) ->
	gen_server:call(Pid, {image, index, Path}).
%%
get_icon(#g{cache = Pid}, Path) when is_binary(Path) ->
	gen_server:call(Pid, {image, icon, Path}).
%%	
get_bitmap(#g{cache = Pid}, Path) when is_binary(Path) ->
	gen_server:call(Pid, {image, bitmap, Path}).
%%
load_image(_, undefined) ->
	undefined;
load_image(#g{cache = Pid}, Path) when is_binary(Path) ->
	gen_server:call(Pid, {image, load, Path}).
	
%%
init([Gx, #gx_cache{constants = Constants, types = Types, modules = Modules, resources = Paths}]) ->
%	?TTY({init, Gx, Paths}),	
	State = #state{
		g = Gx,
		constants = Constants,
		types = Types,
		modules = Modules,
		paths = Paths,
		image_ref = gx_ui_resource:create_image_list(Gx), %% TEMP?
		image_ref32 = gx_ui_resource:create_image_list(Gx, 32, 32),
		images = dict:new(),
		command_index = gx_wx:base_command_id(),
		commands = dict:new(),
		names = dict:new(),
		refs = dict:new(), % digraph?
		ids = dict:new() % refs digraph?
	},
	{ok, State}.
	
%
handle_call({register, Component}, _From, State) ->
	do_register(Component, State);
handle_call({register, Id, Data, Ref}, _From, State = #state{ids = Ids}) ->
	case dict:find(Id, Ids) of
	{ok, {Ref, Data}} ->	
		{reply, {ok, Id}, State};
	{ok, _} ->
		{reply, {error, exists}, State};
	error ->
		Ids0 = dict:store(Id, {Data, Ref}, Ids),
		{reply, {ok, Id}, State#state{ids = Ids0}}
	end;
handle_call({lookup, Name}, _From, State = #state{names = Names}) ->
	case do_lookup(Name, Names) of
	{Ref, _Module} ->
		{reply, Ref, State};
	Value ->
		{reply, Value, State}
	end;
handle_call({reverse_lookup, Ref}, _From, State = #state{refs = Refs}) ->
	Reply = do_lookup(Ref, Refs),
	{reply, Reply, State};
handle_call({lookup_id, Id}, _From, State = #state{ids = Ids}) ->
	Reply = do_lookup(Id, Ids),
	{reply, Reply, State};
handle_call({lookup_type, Type}, _From, State = #state{types = WxGx}) ->
	Reply = do_lookup(Type, WxGx),
	{reply, Reply, State};
handle_call(names, _From, State = #state{names = Names}) ->
	{reply, {ok, dict:to_list(Names)}, State};
handle_call(types, _From, State = #state{modules = GxImpl}) ->
	{reply, dict:to_list(GxImpl), State};
handle_call(paths, _From, State = #state{paths = Paths}) ->
	{reply, Paths, State};
handle_call({image_list, default}, _From, State = #state{image_ref = ImageList}) ->
	{reply, {ok, ImageList}, State};
handle_call({bind_command, Source, Event, Handler}, _From, 
			State = #state{command_index = Index, commands = Commands}) ->
	Commands0 = dict:store(Index, {Source, Event, Handler}, Commands),
	{reply, Index, State#state{command_index = Index + 1, commands = Commands0}};
handle_call({find_command, Index}, _From, State = #state{commands = Commands}) ->
	Reply = do_lookup(Index, Commands),
	{reply, Reply, State};
handle_call({find_module, Type}, _From, State = #state{modules = GxMap}) ->
	Reply = do_lookup(Type, GxMap),
	{reply, Reply, State};
handle_call({lookup_module, GxName}, _From, State = #state{names = Names}) ->
	case dict:find(GxName, Names) of
	{ok, {Ref, Module}} ->
		Reply = {ok, Module, Ref};
	error ->
		Reply = {error, not_found}
	end,
	{reply, Reply, State};	
%
handle_call({image, load, Name}, _From, State = #state{g = G, paths = Paths}) ->
	{reply, load_image(G, Name, Paths), State}; 
handle_call({image, Type, Name}, _From, State = #state{g = G, image_ref = Ref}) ->
	{Index, State0} = find_image_index(G, Name, State),
	case {Type, Index} of
	{_, undefined} ->
		{reply, undefined, State0};
	{index, Index} ->
		{reply, {ok, Index}, State0};
	{icon, Index} ->
		Icon = gx_ui_resource:get_icon(G, Ref, Index),
		{reply, {ok, Icon}, State0};
	{bitmap, Index} ->
		Icon = gx_ui_resource:get_bitmap(G, Ref, Index),
		{reply, {ok, Icon}, State0}
	end;
handle_call(Message, _From, State) ->
	?TTY({handle_call, Message}),
	{reply, ok, State}.
%%
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(Message, State) ->
	?TTY({handle_cast, Message}),
    {noreply, State}.
%%
handle_info(Message, State) ->
	?TTY({handle_info, Message}),
    {noreply, State}.
%%
code_change(_OldVsn, State, _Extra) -> 
	{ok, State}.	
%%
terminate(_Reason, _State = #state{g = #g{port = _Port}, image_ref = _Ref}) ->
%	gx_wx:destroy(Port, Ref), causes badarg at port_control
	ok.

do_register(#gx_ui{id = undefined, ref = Ref, parent = Parent}, State = #state{refs = Refs}) ->
	Refs0 = dict:store(Ref, Parent, Refs),
	{reply, {ok, Ref}, State#state{refs = Refs0}};
do_register(#gx_ui{id = Name, ref = Ref, parent = Parent, module = Module}, 
			State = #state{names = Names, refs = Refs}) ->
	case dict:find(Name, Names) of
	{ok, {Ref, Parent}} ->	
		{reply, {ok, Name}, State};
	{ok, _} ->
		{reply, {error, exists}, State};
	error ->
		Names0 = dict:store(Name, {Ref, Module}, Names),
		Refs0 = dict:store(Ref, Parent, Refs),
		{reply, {ok, Name}, State#state{names = Names0, refs = Refs0}}
	end.
	
do_lookup(Key, Values) ->
	case dict:find(Key, Values) of
	{ok, Value} ->
		Value;
	error ->
		undefined
	end.

% NOTE: this is the icon cache decision and load algorithm
find_image_index(Gx, Name, State = #state{image_ref = Ref, images = Images, paths = Paths}) ->
	case dict:find(Name, Images) of
	{ok, Index} ->
		{Index, State};
	error ->
		case load_image(Gx, Name, Paths) of
		{ok, Bitmap} ->
			Index = gx_ui_resource:append_resource(Gx, Ref, Bitmap),
			ok = gx_wx:destroy(Gx, Bitmap), %% once added to an image list the original is destroyed
			Images0 = dict:store(Name, Index, Images),
			{Index, State#state{images = Images0}};
		undefined ->
			{undefined, State}
		end
	end.
%	
load_image(G, Name, [H|T]) ->
	Path = path:new([H, Name]),
	case path:type(Path) of
	regular ->
		%?TTY({load, Path}),
		Bitmap = gx_ui_resource:load_bitmap(G, Path),
		{ok, Bitmap};
	_ ->
		load_image(G, Name, T)
	end;
load_image(_, _, []) ->
	undefined.
