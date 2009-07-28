%%
%% GX Framework
%% Copyright (c) 2009 Steve Davis <steven.charles.davis@gmail.com>. All rights reserved.
%% LICENSE: Creative Commons Non-Commercial License V 3.0 
%% http://creativecommons.org/licenses/by-nc/3.0/us/
%%
-module(gx_sup).
-vsn("0.3").
-author('steve@simulacity.com').

-behaviour(supervisor).

-export([init/1]).

-export([start_link/1]).


%%
start_link(_) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%
%% Supervisor callbacks
%%

%%
init(_) ->
    GxServer = {gx_srv, {gx_srv, start_link, []}, 
		permanent, 2000, worker, [gx_srv]},
    {ok,{{one_for_all,0,1}, [GxServer]}}.

