%% Copyright 2011 Steve Davis <steve@simulacity.com>
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

-module(gx_ui_stc_autocomplete).

-include("gx.hrl").

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
	code_change/3, terminate/2]).

-export([test/0, test/1, start/0, stop/0]).

test() ->
	test("gx:pkey()").

test([H|T]) ->
	case key(H) of
	ok ->
		ok;
	{ok, M, L} ->
		?TTY({M, L});
	Other ->
		?TTY(Other)
	end,
	test(T);
test([]) ->
	done;
test(Bin) when is_binary(Bin) ->
	test(binary_to_list(Bin)).

%%
start() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
%%
stop() ->
    ok = gen_server:cast(?MODULE, stop).
%%
key(Key) when is_integer(Key) ->
	gen_server:call(?MODULE, {key, Key}).

%
init(_Args) ->
	List = get_ac_list(erlang),
	Dict = dict:append(<<"erlang">>, List, dict:new()),
	{ok, {<<>>, code, Dict}}.
	
% skip comments
handle_call({key, $%}, _From, {_, _, Cache}) ->
	{reply, ok, {<<>>, comment, Cache}};
handle_call({key, $\n}, _From, {_, comment, Cache}) ->
	{reply, ok, {<<>>, code, Cache}};
handle_call({key, _}, _From, State = {_, comment, _}) ->
	{reply, ok, State};
% trigger autocomplete
handle_call({key, $:}, _From, State = {Stack, code, Cache}) ->
	case dict:find(Stack, Cache) of
	{ok, List} ->
		{reply, {ok, Stack, List}, State};
	error ->
		Module = binary_to_atom(Stack, utf8),
		case code:ensure_loaded(Module) of
		{module, Module} ->
			List = get_ac_list(Module),
			Cache0 = dict:append(Stack, List, Cache),
			{reply, {ok, Stack, List}, {<<>>, code, Cache0}};
		E ->
			{reply, {E, Stack}, {<<>>, code, Cache}}
		end
	end;
% build stack
handle_call({key, K}, _From, {Stack, code, Cache}) ->
	case is_char(K) of
	true ->
		{reply, ok, {<<Stack/binary, K>>, code, Cache}};
	false ->
		{reply, ok, {<<>>, code, Cache}}
	end;
% unrecognized
handle_call(Message, _From, State) ->
	?TTY({handle_call, Message}),
	{reply, ok, State}.
%
handle_cast(stop, State) ->
	{stop, normal, State};
handle_cast(Message, State) ->
	?TTY({handle_cast, Message}),
    {noreply, State}.
%
handle_info(Message, State) ->
	?TTY({handle_info, Message}),
    {noreply, State}.
%
code_change(_OldVsn, State, _Extra) -> 
	{ok, State}.	
%
terminate(_Reason, _State) ->
	ok.


is_char(K) when K >= $a andalso K =< $z ->
	true;
is_char(K) when K >= $A andalso K =< $Z ->
	true;
is_char(K) when K =:= $_ ->
	true;
is_char(_) ->
	false.

get_ac_list(Module) ->
	List = lists:sort(Module:module_info(exports)),
	convert([atom_to_binary(F, utf8) || {F, _} <- List], []).

convert([H|T], Acc = [H|_]) ->
	convert(T, Acc);
convert([H = <<C, _/binary>>|T], Acc) when C >= $a andalso C =< $z ->
	convert(T, [H|Acc]);
convert([_H|T], Acc) ->
	convert(T, Acc);
convert([], Acc) ->
	lists:reverse(Acc).
