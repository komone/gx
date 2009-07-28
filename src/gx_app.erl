%%
%% GX Framework
%% Copyright (c) 2009 Steve Davis <steven.charles.davis@gmail.com>. All rights reserved.
%% LICENSE: Creative Commons Non-Commercial License V 3.0 
%% http://creativecommons.org/licenses/by-nc/3.0/us/
%%
-module(gx_app).
-vsn("0.3").
-author('steve@simulacity.com').

-behaviour(application).

-export([start/2, stop/1, get_env/2]).

%%
%% Application callbacks
%%

%%
start(_Type, Args) -> gx_sup:start_link(Args).

%%
stop(_State) -> ok.

%%
get_env(Key, Default) ->
    case application:get_env(gx, Key) of
        {ok, Value} -> Value;
        _           -> Default
    end.
