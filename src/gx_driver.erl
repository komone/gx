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

-module(gx_driver).

-include("gx.hrl").
-include("gx_wx.hrl").

-export([connect/0, release/1]).
%% TEMP?
-export([version/0, users/0, get_cache/0, kill/0]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(DRIVER, wxe_driver).
-define(MINIMUM_ERTS_VERSION, {5, 6, 2}).
-define(TIMEOUT, 5000).

-record(state, {driver = ?DRIVER, version, port, gx_cache}).

%%
connect() ->
	connect(whereis(?MODULE)).
%
connect(undefined) ->
	{ok, Pid} = gen_server:start({local, ?MODULE}, ?MODULE, [], []),
	connect(Pid);
connect(Pid) when is_pid(Pid) ->
    Port = open_port({spawn, ?DRIVER}, [binary]),
    receive 
	wx_port_initiated ->
		ok 
	end,
	Gx = #g{port = Port},
	{ok, StaticCache} = gen_server:call(?MODULE, gx_cache, ?TIMEOUT),
	{ok, Gx, StaticCache}.

%%
release(Gx = #g{port = Port}) ->
	true = port_close(Port),
	case whereis(?MODULE) of
	undefined ->
		ok;
	_ ->
		gen_server:cast(?MODULE, stop)
	end.

%% TEMP?
version() ->
	gen_server:call(?MODULE, wx_version, ?TIMEOUT).

%% TEMP
users() ->
	gen_server:call(?MODULE, wx_users, ?TIMEOUT).

%% TEMP
get_cache() ->
	gen_server:call(?MODULE, gx_cache, ?TIMEOUT).

%% TEMP
kill() ->
	gen_server:cast(?MODULE, kill).

%%
init([]) ->
    true = erlang:system_info(smp_support),
	ErtsVersion = list_to_binary(erlang:system_info(version)),
    true = version:compare(version:decode(ErtsVersion), ?MINIMUM_ERTS_VERSION),
	
    process_flag(trap_exit, true),
	erlang:group_leader(whereis(init), self()),
	
	{ok, Port} = load_wx_driver(),	
	GxCache = bind_gx(),
	{ok, #state{driver = ?DRIVER, port = Port, gx_cache = GxCache}}.
%
handle_call(wx_port, _, State = #state{port = Port}) ->
    {reply, {ok, Port}, State};
handle_call(wx_version, _From, State = #state{version = Version}) ->
	{reply, Version, State};
handle_call(wx_users, _From, State = #state{driver = Driver}) ->
	Reply = {ok, erl_ddll:info(Driver, port_count) - 1},
	{reply, Reply, State};
handle_call(gx_cache, _From, State = #state{gx_cache = GxCache}) ->
	{reply, {ok, GxCache}, State};
handle_call(Message, _From, State) ->
    ?TTY({handle_call, Message}),
    {reply, ok, State}.
%
handle_cast(stop, State = #state{port = _Port}) ->
	case unload_wx_driver() of
%% BUG?: Stopping the driver process causes a bus error on OSX when
%% starting a new driver process. As a workaround, we leave the driver 
%% running once it has been started.
	%true ->
	%	true = port_close(Port),
	%	{stop, normal, State};
	_ ->
		{noreply, State}
	end;
handle_cast(kill, State = #state{port = Port}) ->
	port_close(Port),
	?TTY({erl_ddll:info(?DRIVER, processes), erl_ddll:info(?DRIVER, port_count)}),
	?TTY(erl_ddll:try_unload(?DRIVER, [])),
	{noreply, State};
handle_cast(Message, State) ->
    ?TTY({handle_cast, Message}),
    {noreply, State}.
%
handle_info({wx_consts, WxConstants}, State = #state{gx_cache = GxCache}) ->
	Constants = dict:from_list(WxConstants),
	Version = {
		dict:fetch(wxMAJOR_VERSION, Constants),
		dict:fetch(wxMINOR_VERSION, Constants),
		dict:fetch(wxRELEASE_NUMBER, Constants),
		dict:fetch(wxSUBRELEASE_NUMBER, Constants)
	},
	{noreply, State#state{version = Version, gx_cache = GxCache#gx_cache{constants = Constants}}};
handle_info(Message, State) ->
    ?TTY({handle_info, Message}),
    {noreply, State}.
%
terminate(normal, #state{port = Port}) ->
	?TTY({stopped, Port}),
	%erl_ddll:unload_driver(?DRIVER),
	ok.
%
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%
load_wx_driver() ->	
	PrivDir = code:priv_dir(wx),
%	set_path(PrivDir),
	?TTY(PrivDir),
	case erl_ddll:load_driver(PrivDir, ?DRIVER) of
	{error, Reason} -> 
	    error_logger:format("GX driver failed load ~p@~p ~n", [?DRIVER, PrivDir]),
	    Message = erl_ddll:format_error(Reason),
	    erlang:error({load_driver, Message});
	ok -> 
		%DriverWithArgs = atom_to_list(?DRIVER) ++ " " ++ PrivDir ++ [0],
		case open_port({spawn, ?DRIVER}, [binary]) of
		{'EXIT', Error} -> 
			erlang:error({open_port, Error});
		Port ->
			{ok, Port}
		end
    end.
%%
unload_wx_driver() ->
	Self = self(),
	case {erl_ddll:info(?DRIVER, processes), erl_ddll:info(?DRIVER, port_count)} of
	{[{Self, _}], 1} ->
		true;
	_ ->
		false
	end.

%% NOTE: Design trade-off made here
%  Using cached values is less efficient than a static "function map", but  
%  it eases error-free addition/update of wx components to gx.
bind_gx() ->
	GxModules = code_util:find_modules(gx, gx_ui),
	Mapping = lists:flatten([X:mapping_info() || X <- GxModules]),
	Types = bind_wx_types(Mapping, dict:new()),
	Modules = bind_gx_modules(Mapping, dict:new()),
	DefaultResourcePath = path:new([code:lib_dir(gx), ?DEFAULT_RESOURCE_PATH]),
	#gx_cache{types = Types, modules = Modules, resources = [DefaultResourcePath]}.

bind_wx_types([#gx_wx{wx_type = undefined}|T], Dict) ->
	bind_wx_types(T, Dict);
bind_wx_types([#gx_wx{type = Type, wx_type = WxType}|T], Dict) ->
	bind_wx_types(T, dict:store(WxType, Type, Dict));
bind_wx_types([], Dict) ->
	Dict.
	
bind_gx_modules([#gx_wx{type = Type, module = Module}|T], Dict) ->
	bind_gx_modules(T, dict:store(Type, Module, Dict));
bind_gx_modules([], Dict) ->
	Dict.

