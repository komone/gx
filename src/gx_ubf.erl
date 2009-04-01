%%
%% GX Framework
%% Copyright (c) 2009 Steve Davis <steven.charles.davis@gmail.com>. All rights reserved.
%% LICENSE: Creative Commons Non-Commercial License V 3.0 
%% http://creativecommons.org/licenses/by-nc/3.0/us/
%%
-module(gx_ubf).
-vsn("0.3").
-author('steve@simulacity.com').

-include("../include/gx.hrl").

-export([encode/1, decode/1]).


encode(Term = #gx{}) ->
	ubf:encode(Term).

decode(UBF) ->
	{done, Term, []} = ubf:decode(UBF),
	Term.

%placeholder
%parse_term(Term) -> Term.
	
