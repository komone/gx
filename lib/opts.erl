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

-module(opts).

%-include("ice.hrl").
-export([get/2, get/3, take/2, set/2, set/3, normalize/1, from_record/2, to_record/3]).

%%
get(Key, Opts) ->
	get(Key, Opts, undefined).

%%
get(Key, Opts, Default) when is_list(Opts) ->
	case lists:keyfind(Key, 1, Opts) of
	{Key, Value} ->
		Value;
	false ->
		Default
	end.

%%
take(Key, Opts)->
	case lists:keytake(Key, 1, Opts) of
	{Key, Value, Opts0} ->
		{Value, Opts0};
	false ->
		{undefined, Opts}
	end.

%%
set(Key, Opts) ->
	opts:set({Key, true}, Opts).

%%
set(Key, Value, Opts) when is_list(Opts) ->
	lists:keystore(Key, 1, Opts, {Key, Value}).
	
%%
normalize(Opts) ->
	normalize(Opts, []).
%
normalize([H|T], Acc) when is_atom(H) ->
	normalize([{H, true}|T], Acc);
normalize([{K, V}|T], Acc) when is_atom(K), is_list(V) ->
	Value = 
		case text:is_string(V) of
		true ->
			list_to_binary(V);
		false ->
			normalize(V, [])
		end,
	normalize(T, lists:keystore(K, 1, Acc, {K, Value}));
normalize([H = {K, _V}|T], Acc) when is_atom(K) ->
	normalize(T, lists:keystore(K, 1, Acc, H));
normalize([], Acc) ->
	lists:sort(lists:reverse(Acc)).

%%
to_record(RecordName, Fields, Opts) when is_atom(RecordName) ->
	to_record0(Fields, Opts, [RecordName]).
%
to_record0([H|T], Opts, Acc) ->
	{value, {H, V}, Opts0} = lists:keytake(H, 1, Opts),
	to_record0(T, Opts0, [V|Acc]);
to_record0([], [], Acc) ->
	list_to_tuple(lists:reverse(Acc)).

%%
from_record(Record, Fields) when is_tuple(Record), is_list(Fields), 
		size(Record) =:= length(Fields) + 1 ->
	[_Type | Values] = tuple_to_list(Record),
	from_record(Fields, Values, []).
%
from_record([_|KT], [undefined|VT], Acc) ->
	from_record(KT, VT, Acc);
from_record([K|KT], [V|VT], Acc) ->
	from_record(KT, VT, [{K, V}|Acc]);
from_record([], [], Acc) ->
	lists:reverse(Acc).
