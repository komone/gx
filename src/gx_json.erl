%%
%% GX Framework
%% Copyright (c) 2009 Steve Davis <steven.charles.davis@gmail.com>. All rights reserved.
%% LICENSE: Creative Commons Non-Commercial License V 3.0 
%% http://creativecommons.org/licenses/by-nc/3.0/us/
%%
-module(gx_json).
-vsn("0.3").
-author('steve@simulacity.com').

-include("../include/gx.hrl").
-export([encode/1, decode/1]).
-compile(export_all).

-define(JSON_TOKENIZER, "(\"[^\"]*\"|[{}:,\\[\\]])").
-define(JSON_NUMBER, "^([-]?(0|[1-9][0-9]*)(\.[0-9]+)?([Ee][+-]?[0-9]+)?)$").
-define(JSON_STRING, "(\"[^\"]*\")").



test() ->
	Term = #gx{},
	Term = decode(encode(Term)).

%
encode_ui(T = #gx{}) ->
	Term = {gx, [{vsn, "0.3"}, {ui, [T]}]},
	encode(Term).

encode(Term) when is_tuple(Term) ->
	encode(Term, []).
encode(Term = {_K, _V}, _Acc) ->	
	[String, Int, Long] = Term#gx.data,
	
	Id = case Term#gx.id of
	Value when is_integer(Value) -> integer_to_list(Value);
	Value when is_atom(Value) -> [$", atom_to_list(Value), $"]
	end,
	lists:flatten([
		"{\"gx\": ",
			"[\"id\":", Id, ",",
			"\"type\":\"", atom_to_list(Term#gx.type), "\",",
			"\"event\":\"", atom_to_list(Term#gx.event), "\",",
			"\"data\":[\"", 
				String,"\",", 
				integer_to_list(Int), ",",
				integer_to_list(Long), "],",
			"\"user\":", io_lib:format("~w", [Term#gx.user]), ","
			"\"wx\":", io_lib:format("~w", [Term#gx.wx]),
			"]}"]).

% decode/1
decode(Json) when is_binary(Json) ->
	decode(binary_to_list(Json));
decode(Json) ->
	Tokens = tokenize(Json),
	io:format("TOKENS~n~p~n", [Tokens]),
	Term = decode(Tokens, []),
	Term.

% decode/2
decode(["{","}"|Tokens], Acc) -> % the empty object
	decode(Tokens, Acc);
decode(["{"|Tokens], Acc) ->
	io:format("OBJECTSTART~n", []),
	{Term, Rest} = decode(Tokens, []),
	io:format("OBJECTEND~n", []),
	decode(Rest, [Term|Acc]);
decode(["}"|Tokens], Acc) ->
	io:format("OBJECT ~p~n", [list_to_tuple(lists:reverse(Acc))]),
	{list_to_tuple(lists:reverse(Acc)), Tokens};
decode(["[","]"| Tokens], Acc) -> 
	decode(Tokens, Acc);
decode(["["|Tokens], Acc) ->
	io:format("ARRAYSTART~n", []),
	{Term, Rest} = decode(Tokens, []),
	io:format("ARRAYEND~n", []),
	decode(Rest, [Term|Acc]);
decode(["]"|Tokens], Acc) -> 
	io:format("ARRAY ~p~n", [lists:reverse(Acc)]),
	{lists:reverse(Acc), Tokens};
decode([Key, ":"|Tokens], Acc) ->
	io:format("KEY     ~p~n", [Key]),
	{Term, Rest} = 
		case Tokens of
		["{"|_] -> decode(Tokens, []);
		["["|_] -> decode(Tokens, []);
		[H|_] -> decode([H], [])
		end,
	Pair = {atomize(Key), Term},
	io:format("PAIR    ~p~n", [Pair]),
	decode(Rest, [Pair|Acc]);
decode([","|Tokens], Acc) -> 
	decode(Tokens, Acc); % ignored -- correct?
decode(["true"|Tokens], Acc) -> 
	decode(Tokens, [true|Acc]);
decode(["false"|Tokens], Acc) -> 
	decode(Tokens, [false|Acc]);
decode(["null"|Tokens], Acc) -> 
	decode(Tokens, [null|Acc]);
decode([String = [$"|_] | Tokens], Acc) ->
	Value = decode_string(String),
	decode(Tokens, [Value|Acc]);
decode([Number|Tokens], Acc) ->
	Value = decode_number(Number),
	decode(Tokens, [Value|Acc]);
decode([], Acc) ->
	lists:reverse(Acc).

%
tokenize(Json) ->
	List = re:split(Json, ?JSON_TOKENIZER, [{return, list}]),
	[X || X <- lists:map(fun(X) -> trim(X) end, List), X =/= []].
%
trim(String) -> 
	String1 = trim1(lists:reverse(String)),
	trim1(lists:reverse(String1)).
trim1([$\t|T]) -> trim1(T);
trim1([$\r|T]) -> trim1(T);
trim1([$\n|T]) -> trim1(T);
trim1([$ |T]) -> trim1(T);
trim1(T) -> T.


atomize(Token) ->
	[[], String, []] = re:split(Token, "\"", [{return, list}]),
	list_to_atom(String).
	
%
decode_string(Token) ->
% TODO: deal with escape sequences, check chars are unicode
	[[], String, []] = re:split(Token, "\"", [{return, list}]),
	String.

% JSON numbers do not have quite the same spec as Erlang numbers.
decode_number(Token) ->
	Number = trim(Token),
	true = is_match(Number, ?JSON_NUMBER), %% force error
	% IMPL: JSON allows exponent notation for integers
	case is_match(Number, "[\.]") of
	true -> list_to_float(Number);
	false -> 
		case re:split(Number, "[Ee]", [{return, list}]) of
		[M, E] -> list_to_float(lists:append([M,".0e", E]));
		[_] -> list_to_integer(Number)
		end
	end.

%
is_match(String, Regex) ->
	case re:run(String, Regex) of
	match -> true;
	{match, _} -> true;
	nomatch -> false
	end.
