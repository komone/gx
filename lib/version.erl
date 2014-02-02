-module(version).


-export([encode/1, decode/1, compare/2]).
	
decode(Bin) when is_binary(Bin) ->
	List0 = text:split(Bin, <<"\\.">>),
	List1 = [text:to_number(X) || X <- List0],
	list_to_tuple(List1).
	
encode(Term) when is_tuple(Term) ->
	List0 = tuple_to_list(Term),
	List1 = [text:encode(X) || X <- List0, is_integer(X)],
	text:from_list(List1, <<$.>>).

compare(V, V0) when is_tuple(V), is_tuple(V0) ->
	tuple_to_list(V) >= tuple_to_list(V0).
