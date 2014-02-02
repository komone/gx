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

-module(text).

%-include("ice.hrl").

-export([format/1, encode/1, encode_list/1, trim/1, split/2, split/3, replace/3, strip/2, 
	unquote/1, matches/2, is_string/1, is_upper/1, is_lower/1, to_upper/1, 
	to_lower/1, to_number/1, from_list/1, from_list/2, get_token/2]).
-export([distance/2]).
-export([get_integer/1, parse_integer/1, parse_integers/1, parse_float/1, from_csv/1]).

-define(CSV_REGEX, <<", (?=(?:[^\"]*\"[^\"]*\")*(?![^\"]*\"))">>).

-define(is_string(X), is_list(X) andalso is_integer(hd(X))).

from_csv(Bin) ->
	Lines = split(Bin, <<"[\r\n]+">>),
	[H|T] = from_csv(Lines, []),
	{H, T}.

from_csv([H|T], Acc) ->
	L = parse_csv(H, false, <<>>, []),
	from_csv(T, [list_to_tuple(L)|Acc]);
from_csv([], Acc) ->
	lists:reverse(Acc).

parse_csv(<<$\\, $", Bin/binary>>, true, V, Acc) ->
	parse_csv(Bin, true, <<V/binary, $">>, Acc);
parse_csv(<<$", Bin/binary>>, Esc, V, Acc) ->
	parse_csv(Bin, Esc =:= false, V, Acc);
parse_csv(<<$,, Bin/binary>>, false, V, Acc) ->
	parse_csv(Bin, false, <<>>, [V|Acc]);
parse_csv(<<X, Bin/binary>>, Esc, V, Acc) ->
	parse_csv(Bin, Esc, <<V/binary, X>>, Acc);
parse_csv(<<$,>>, _, V, Acc) ->
	lists:reverse([<<>>, V|Acc]);
parse_csv(<<>>, _, V, Acc) ->
	lists:reverse([V|Acc]).


encode_list(L) when is_list(L) ->
	[encode(X) || X <- L].

%%
encode(X) when is_binary(X)  -> 
	X;
encode(X) when is_list(X) -> 
	case is_string(X) of
	true -> 
		X0 = X;
	false ->
		X0 = io_lib:format("~w", [X])
	end,
	list_to_binary(X0);
encode(X) ->
	list_to_binary(io_lib:format("~w", [X])).

%%
format(X) when is_atom(X) ->
	atom_to_binary(X, utf8);
format(X) when is_integer(X) ->
	list_to_binary(integer_to_list(X));
format(X) when is_float(X) ->
	list_to_binary(float_to_list(X));
format(X) when is_pid(X) ->
	list_to_binary(erlang:pid_to_list(X));
format(X) when is_port(X) ->
	list_to_binary(erlang:port_to_list(X));
format(X) when is_reference(X) ->
	list_to_binary(erlang:ref_to_list(X));
format(X) when is_function(X) ->
	list_to_binary(erlang:fun_to_list(X));
format(X) when is_binary(X) ->
	case is_string(X) of
	true ->
		C = list_to_binary([$", X, $"]);
	false ->
		C = format(binary_to_list(X), [])
	end,
	list_to_binary([$<, $<, C, $>, $>]);
format(X) when is_tuple(X) ->
	L = tuple_to_list(X),
	list_to_binary([${, format(L, []), $}]);
format([]) ->
	list_to_binary([$[,$]]);
format(X) when is_list(X) ->
	case is_string(X) of
	true ->
		list_to_binary([$", X, $"]);
	false ->
		list_to_binary([$[, format(X, []), $]])
	end.
% list to binary csv
format([], Acc) ->
	list_to_binary(lists:reverse(Acc));
format([H], Acc) ->
	list_to_binary(lists:reverse([format(H) | Acc]));
format([H|T], Acc) ->
	format(T, [$,, format(H) | Acc]).

is_string(Bin) when is_binary(Bin) ->
	is_string(binary_to_list(Bin));
is_string([H|T]) when H =:= 16#9; H =:= 16#a; H =:= 16#d ->
	is_string(T);
is_string([H|T]) when H >= 16#20, H =< 16#7e ->
	is_string(T);
is_string([H|T]) when H >= 16#a0, H =< 16#ff ->
	is_string(T);
is_string([]) ->
	true;
is_string(_) ->
	false.

%% Currently, just simple integers and floats
to_number(X) ->
	X1 = trim(X),
	case X1 of
	<<".", Rest/binary>> ->
		X2 = <<"0.", Rest/binary>>;
	Value ->
		X2 = Value
	end,
	case matches(X2, <<"^[+-]?[0-9]*\\.[0-9]+?$">>) of
	true ->
		list_to_float(binary_to_list(<<X2/binary>>));
	false ->
		case matches(X2, <<"^[+-]?[0-9]+$">>) of
		true ->
			list_to_integer(binary_to_list(X2));
		false ->
			undefined
		end
	end.

%%
matches(Bin, Regex) when is_binary(Bin) ->
	matches(Bin, Regex, byte_size(Bin));
matches(Bin, Regex) when is_list(Bin) ->
	matches(Bin, Regex, length(Bin)).
%
matches(Bin, Regex, Size) ->
	case re:run(Bin, Regex) of
	{match, [{0, Size}]} ->
		true;
	_ ->
		false
	end.
	
get_token(Bin, Regex) ->
	case re:run(Bin, Regex) of
	{match, [{0, N}]} ->
		<<Token:N/binary, Rest/binary>> = Bin,
		{Token, Rest};
	_ ->
		{<<>>, Bin}
	end.
%%
split(Bin, Regex) ->
	split(Bin, Regex, []).
split(Bin, Regex, Parts) when is_integer(Parts) ->
	List = split(Bin, Regex, [{parts, Parts}]),
	lists:append(List, lists:duplicate(Parts - length(List), <<>>));
split(Bin, Regex, all) ->
	re:split(Bin, Regex, []);
split(Bin, Regex, Opts) ->
	[X || X <- re:split(Bin, Regex, Opts), X =/= <<>>].	

%%
replace(Bin, Regex, Value) ->
	re:replace(Bin, Regex, Value, [{return, binary}, global]).

%%
strip(Bin, Regex) ->
	replace(Bin, Regex, <<>>).

get_integer(<<$-, Bin/binary>>) ->
	{Integer, Bin0} = get_integer(Bin, 0),
	{-1 * Integer, Bin0};
get_integer(Bin) ->
	get_integer(Bin, 0).
%
get_integer(<<X, Bin/binary>>, Acc) when X >= $0 andalso X =< $9 ->
	Acc0 = Acc * 10 + (X - $0),
	get_integer(Bin, Acc0);
get_integer(Bin, Acc) ->
	{Acc, Bin}.

parse_integers(L) when is_list(L) ->
	[parse_integer(X) || X <- L].

%%
parse_integer(<<$-, Bin/binary>>) ->
	-1 * parse_integer(Bin, 0);
parse_integer(Bin) when is_binary(Bin) ->
	parse_integer(Bin, 0).
%
parse_integer(<<X, Bin/binary>>, Acc) when X >= $0 andalso X =< $9 ->
	Acc0 = Acc * 10 + (X - $0),
	parse_integer(Bin, Acc0);
parse_integer(<<>>, Acc) ->
	Acc.

%%
parse_float(<<$-, Binary/binary>>) ->
	-1 * parse_float(Binary, 0.0);
parse_float(Binary) ->
	parse_float(Binary, 0.0).
%
parse_float(<<$., Binary/binary>>, Acc) ->
	Acc + parse_integer(Binary) / math:pow(10, byte_size(Binary));
parse_float(<<X, Rest/binary>>, Acc) when X >= $0 andalso X =< $9 ->
	Acc0 = Acc * 10 + (X - $0),
	parse_float(Rest, Acc0);
parse_float(<<>>, Acc) ->
	Acc.

%%
unquote(Bin) ->
	replace(Bin, <<"^\"|\"$">>, <<"">>).

%%
from_list(L) ->
	from_list(L, <<" ">>).
from_list([H|T], Separator) when is_integer(H); is_float(H) ->
	from_list(T, Separator, [encode(H)]);
from_list([H|T], Separator) ->
	from_list(T, Separator, [H]);
from_list([], _) ->
	[].
	
from_list([H|T], Separator, Acc) when is_integer(H); is_float(H) ->
	from_list(T, Separator, [encode(H), Separator | Acc]);
from_list([H|T], Separator, Acc) ->
	from_list(T, Separator, [H, Separator | Acc]);
from_list([], _, Acc) ->
	list_to_binary(lists:reverse(Acc)).

%% hmmm
trim(S) when ?is_string(S) ->
	trim(list_to_binary(S));
trim(L) when is_list(L) ->
	T = [trim(X) || X <- L],
	[X || X <- T, X =/= <<>>];
trim(S) when is_binary(S) -> 
	% @thanks Seth Falcon
	replace(S, <<"^\\s+|\\s+$">>, <<"">>);
trim(S) -> 
	S.

%%
is_upper(<<C>>) -> is_upper(C);
is_upper([C]) -> is_upper(C);
is_upper(C) when C >= $A, C =< $Z -> true;
is_upper(C) when C >= 16#C0, C =< 16#D6 -> true;
is_upper(C) when C >= 16#D8, C =< 16#DE -> true;
is_upper(_) -> false.

%% 
to_upper(C) when is_integer(C) -> to_upper(<<C>>);
to_upper(S) when ?is_string(S) -> string:to_upper(S);
to_upper(B) when is_binary(B) -> bin_to_upper(B, <<>>);
to_upper(S) -> S.
% @private
bin_to_upper(<<C, Rest/binary>>, Acc) ->
	U = uppercase(C),
	bin_to_upper(Rest, <<Acc/binary, U>>);
bin_to_upper(<<>>, Acc) ->
	Acc.
%% IMPL: Latin1
uppercase(C) when C >= $a, C =< $z -> C - 32;
uppercase(C) when C >= 16#E0, C =< 16#F6 -> C - 32;
uppercase(C) when C >= 16#F8, C =< 16#FE -> C - 32;
uppercase(C) -> C.

%% 
is_lower(<<C>>) -> is_lower(C);
is_lower([C]) -> is_lower(C);
is_lower(C) when C >= $a, C =< $z -> true;
is_lower(C) when C >= 16#E0, C =< 16#F6 -> true;
is_lower(C) when C >= 16#F8, C =< 16#FE -> true;
is_lower(_) -> false.
	
%%
to_lower(C) when is_integer(C) -> lowercase(C);
to_lower(S) when ?is_string(S) -> string:to_lower(S);
to_lower(B) when is_binary(B) -> bin_to_lower(B, <<>>);
to_lower(V) -> V.
% 
bin_to_lower(<<C, Rest/binary>>, Acc) ->
	C1 = lowercase(C),
	bin_to_lower(Rest, <<Acc/binary, C1>>);
bin_to_lower(<<>>, Acc) ->
	Acc.
%% IMPL: Latin1	
lowercase(C) when C >= $A, C =< $Z -> C + 32;
lowercase(C) when C >= 16#C0, C =< 16#D6 -> C + 32;
lowercase(C) when C >= 16#D8, C =< 16#DE -> C + 32;
lowercase(C) -> C.

%% @ref Levenshtein Distance <http://en.wikipedia.org/wiki/Levenshtein_distance>
%% TODO: transpositions (Demerau-Levenshtein)
%% @ref Damerau-Levenshtein Distance <http://en.wikipedia.org/wiki/Damerau%E2%80%93Levenshtein_distance>
distance(Bin, Bin) ->
	0;
distance(<<>>, BinB) ->
	byte_size(BinB);
distance(BinA, <<>>) ->
	byte_size(BinA);
distance(BinA, BinB) ->
	Init = lists:seq(0, byte_size(BinB)),
	levenshtein(BinA, BinB, Init, 1).
%
levenshtein(<<CharA, BinA/binary>>, BinB, Previous, Count) ->
	Next = levenshtein(CharA, BinB, Previous, [Count], Count),
	levenshtein(BinA, BinB, Next, Count + 1);
levenshtein(<<>>, _, Previous, _) ->
	lists:last(Previous).
%	
levenshtein(A, <<A, BinB/binary>>, [H|T], Next, _) ->
	levenshtein(A, BinB, T, [H|Next], H);
levenshtein(A, <<_, BinB/binary>>, [H, H1|T], Next, Count) ->
	Min = lists:min([H + 1, H1 + 1, Count + 1]),
	levenshtein(A, BinB, [H1|T], [Min|Next], Min);
levenshtein(_, <<>>, _, Next, _) ->
	lists:reverse(Next).

