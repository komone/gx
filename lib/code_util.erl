%% Copyright 2009-2014 Steve Davis <steve@simulacity.com>
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

-module(code_util).

-export([find_modules/2, check_behaviour/2, check_dependencies/1, code_clashes/0]).
-export([ftime/2, timing/2]).

find_modules(Application, Behaviour) ->
	{module, Application} = code:ensure_loaded(Application),
	Path = path:new(code:lib_dir(Application, ebin)),
	BeamFiles = path:list(Path, beam),
	Modules = [binary_to_atom(path:basename(X), utf8) || X <- BeamFiles],
	[X || X <- Modules, check_behaviour(X, Behaviour) =:= ok].
	
%%
check_behaviour(Module, Behaviour) when is_atom(Module), is_atom(Behaviour) ->
	case code:ensure_loaded(Module) of 
	{module, Module} ->
		Info = Module:module_info(attributes),
		List  = [Y || {X, Y} <- Info, X =:= 'behaviour' orelse X =:= 'behavior'],
		case [X || X <- lists:flatten(List), X =:= Behaviour] of
		[Behaviour] -> 
			ok;
		[] ->
			{error, not_found}
		end;
	Error -> Error
	end.

%%
check_dependencies(Depends) ->
	case [X || X <- Depends, is_pid(whereis(X)) =/= true] of
	[] -> 
		ok;
	NotRunning -> 
		throw({depends, NotRunning})
	end.

%%
code_clashes() ->
    Path = code:get_path(),
	Modules = [path:list(X, beam) || X <- Path, path:type(X) =:= directory],
	List = [{path:name(X), X} || X <- lists:flatten(Modules)],
	find_clashes(List, []).
%	
find_clashes([{Name, Path}|T], Acc) ->
	case lists:keyfind(Name, 1, T) of
	false ->
		find_clashes(T, Acc);
	{Name, Path0} ->
		find_clashes(T, [{binary_to_atom(Name, utf8), Path, Path0}|Acc])
	end;
find_clashes([], Acc) ->
	lists:reverse(Acc).

%%
ftime(F, Repeats) when is_function(F, 0) ->
	Begin = erlang:now(),
    exec_fun(F, Repeats),
	End = erlang:now(),
	timing(Begin, End);
ftime({M, F, A}, Repeats) when is_integer(Repeats) ->
	Begin = erlang:now(),
    apply_fun({M, F, A}, Repeats),
	End = erlang:now(),
	timing(Begin, End).
%
exec_fun(_, 0) ->
	ok;
exec_fun(F, Count) ->
	F(),
	exec_fun(F, Count - 1).
%
apply_fun(_, 0) -> 
	ok;
apply_fun(T = {M, F, A}, Repeats) ->
	apply(M, F, A),
	apply_fun(T, Repeats - 1).
%
timing({A1, B1, C1}, {A2, B2, C2}) ->
	((A2-A1) * 1000000 + (B2-B1)) * 1000 + (C2-C1) / 1000.
