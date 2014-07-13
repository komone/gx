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

-module(gx_srv).

-include("gx.hrl").
-include("gx_wx.hrl").

-export([start/1, stop/0, stop/1, context/0, context/1, bind_event/3]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {context, callback, user_state}).

%%
start(#gui{id = Name, module = Callback, path = Path, def = Defs}) when is_atom(Name), Name =/= undefined ->
	%T0 = erlang:now(),
	case gen_server:start({local, Name}, ?MODULE, [Name, Callback, Path], []) of
    {ok, Pid} ->	
		{ok, Gx} = gen_server:call(Pid, context, infinity),
		gx_ui:create_tree(Gx, gx_wx:null(), Defs),
		% Putting the context in the PD is to allow the use of GX from REPL (may remove).
		put(gx, Gx),
		gen_server:cast(Pid, init_handler),
		{ok, Pid};
	Other ->
		Other
	end.

%%
stop() ->
	stop(context()).
%%
stop(Name) when is_atom(Name) ->
	case whereis(Name) of
	Pid when is_pid(Pid) ->
		gen_server:cast(Pid, stop);
	Value ->
		Value
	end;
%
stop(#g{server = Pid}) ->
    gen_server:cast(Pid, stop).

%%
context() ->
	case get(gx) of
	G = #g{server = Pid} ->
		case is_process_alive(Pid) of
		true ->
			G;
		false ->
			G = erase(gx),
			undefined
		end;
	Other ->
		Other
	end.

%%
context(Name) when is_atom(Name) ->
	case whereis(Name) of
	Pid when is_pid(Pid) ->
		{ok, Context} = gen_server:call(Pid, context, infinity),
		put(gx, Context),
		ok;
	Value ->
		Value
	end.

bind_event(#g{server = Pid}, Ref, Event) ->
	gen_server:cast(Pid, {bind_event, Ref, Event}).

%%
init([Name, Module, Path]) ->
    {ok, InitialContext, InitialCache} = gx_driver:connect(),
	GxCache = 
		case path:type(Path) of
		directory ->
			#gx_cache{resources = Paths} = InitialCache,
			InitialCache#gx_cache{resources = [Path|Paths]};
		_ ->
			InitialCache
		end,
	{ok, Pid} = gx_cache:start_link(InitialContext#g{server = self()}, GxCache),
	Gx = InitialContext#g{server = self(), cache = Pid, name = Name},
	?TTY(Gx),
	{ok, #state{context = Gx, callback = Module, user_state = [{resources, Path}]}}.
%%
handle_call(context, _From, State = #state{context = Gx}) ->
	{reply, {ok, Gx}, State};
handle_call(Message, _From, State) ->
	?TTY({handle_call, Message}),
    {reply, ok, State}.
%%
handle_cast(init_handler, State = #state{context = Gx, callback = Module, user_state = UserState}) ->
	#g{name = Name} = Gx,
	InitialEvent = #gx{id = Name, ref = gx_wx:null(), type = gx, event = start, data = []},
	{_Pid0, _Monitor} = do_callback(Gx, Module, init, InitialEvent, UserState),
	{noreply, State};
handle_cast({bind_event, Ref, Event}, State = #state{context = Gx}) ->
%	?TTY({bind, Gx, Ref, Event}),
	{ok, #wx_ref{}} = gx_event:bind(Gx, Ref, Event),
	{noreply, State};
handle_cast(stop, State) ->
	{stop, normal, State};
handle_cast(Message, State) ->
	?TTY({handle_cast, Message}),
    {noreply, State}.
%%
handle_info(Message = {gx_call, _}, State) ->
	?TTY(Message),
	{noreply, State};
handle_info({gx_callback, {exit, UserState}}, State) ->
	{stop, normal, State#state{user_state = UserState}};
handle_info({gx_callback, {ok, UserState}}, State) ->
	{noreply, State#state{user_state = UserState}};
handle_info({'DOWN', _MonitorRef, process, _Pid, normal}, State) ->
	{noreply, State};
handle_info({'DOWN', MonitorRef, process, Pid, Reason}, State) ->
	?TTY({error, Pid, MonitorRef, Reason}),
    {noreply, State};
handle_info(Message, State = #state{context = G, callback = Module, user_state = UserState}) ->
%	?TTY({handle_event, Message}),
	case gx_event:decode(G, Message) of
	{Event = #gx{event = close_window}, local} ->
		?TTY(Event),
		{stop, normal, State};
	{Event = #gx{}, undefined} ->
		?TTY({unknown_event, Event}),
		{noreply, State};
	{Event = #gx{}, local} ->
		?TTY({no_handler, Event}),
		{noreply, State};
	{Event = #gx{}, context_menu} ->
		%?TTY({context_menu, Event}),
		{_Pid, _Monitor} = do_call(G, gx_ui_menu, context_menu, Event),
		{noreply, State};
	{Event = #gx{}, Handler} when is_atom(Handler) ->
		{_Pid, _Monitor} = do_callback(G, Module, Handler, Event, UserState),
		{noreply, State};
	{Event = #gx{}, {CustomModule, Handler}} ->
		{_Pid, _Monitor} = do_callback(G, CustomModule, Handler, Event, UserState),
		{noreply, State};
	_ ->
		?TTY({handle_info, Message}),
		{noreply, State}
	end.
%%
terminate(normal, #state{context = Gx, callback = Module, user_state = UserState}) ->
	#g{name = Name} = Gx,
	TerminateEvent = #gx{id = Name, ref = gx_wx:null(), type = gx, event = terminate, data = []},
	{_Pid0, _Monitor} = do_callback(Gx, Module, terminate, TerminateEvent, UserState),
	ok = gx_cache:stop(Gx),
	ok = gx_driver:release(Gx);
%
terminate(Reason, #state{context = Context}) ->
	ok = gx_cache:stop(Context),
	gx_driver:release(Context),
	?TTY({terminate, Context, Reason}),
    shutdown.
%%
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%
do_callback(G, Module, Handler, Event, UserState) ->
	Self = self(),
	%?TTY({do_callback, Self, [G, Module, Handler, Event, UserState]}),
	F = fun() -> 
			put(gx, G),
			Result = apply(Module, Handler, [Event, UserState]),
			Self ! {gx_callback, Result}
		end,
	{_Pid, _Monitor} = spawn_monitor(F).	

%%
do_call(G, Module, Handler, Event) ->
	Self = self(),
	F = fun() -> 
			Result = apply(Module, Handler, [G, Event]),
			Self ! {gx_call, Result}
		end,
	{_Pid, _Monitor} = spawn_monitor(F).	

