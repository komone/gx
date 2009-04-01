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
	[String, Int, Long] = Term#gx.data,
	
	Id = case Term#gx.id of
	Value when is_integer(Value) -> integer_to_list(Value);
	Value when is_atom(Value) -> [$", atom_to_list(Value), $"]
	end,
	
	lists:flatten([
		"{\"gx\": ",
			"[\"id\": ", Id, ",",
			" \"type\": \"", atom_to_list(Term#gx.type), "\",",
			" \"event\": \"", atom_to_list(Term#gx.event), "\",",
			" \"data\": [\"", 
				String, "\",", 
				integer_to_list(Int), ",",
				integer_to_list(Long), "]",
			" \"user\": ", parse_term(Term#gx.user), ""
		"]}"]).

decode(UBF) ->
	{ok, UBF}.

parse_term(Term) ->
	Term.
	
