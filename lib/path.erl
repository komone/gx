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

-module(path).

-include_lib("kernel/include/file.hrl").
-include_lib("stdlib/include/zip.hrl").

-export([new/0, new/1, parent/1, name/1, appname/1, extension/1]).
-export([open/2, close/1, basename/1]).
-export([load/1, save/2, eval/1]).
-export([list/0, list/1, list/2, exists/1, code_path/1]). 
-export([type/1, modified/1, ensure/1]).
-export([file_info/1, find/2]).

-define(PATH_SEPARATOR, <<"[\\\\/]">>).
-define(BIN_DIR, <<"ebin">>).

-define(is_string(X), is_list(X) andalso is_integer(hd(X))).

%%
new() ->
	new(<<".">>).
%% NOTE: /.. -> /
new(Bin) when is_binary(Bin) ->
	new([Bin]);
new(List) when ?is_string(List) -> % "strings"
	new([list_to_binary(List)]);
new(Atom) when is_atom(Atom) ->
	new([Atom]);
new(BinList) ->	
	{ok, CurrentDir} = erl_prim_loader:get_cwd(),
	Filtered = filter_paths(BinList, [list_to_binary(CurrentDir)]),
	Parts = [text:split(X, ?PATH_SEPARATOR) || X <- Filtered],
%	?TTY({parts, lists:append(Parts)}),
	make_path(lists:append(Parts), []).

% convert atoms to an app directory path (TODO: consider submodules)
filter_paths([H|T], Acc) when is_atom(H) ->
	{'module', App} = code:ensure_loaded(H),
	{file, Path} = code:is_loaded(App),
	filter_paths([list_to_binary(Path), <<"../..">> | T], Acc);
%% convert "string" elements 
filter_paths([H|T], Acc) when ?is_string(H) ->
	filter_paths([list_to_binary(H)|T], Acc);
%% drop elements that precede an absolute path
filter_paths([H = <<$/, _/binary>>|T], _Acc) ->
	filter_paths(T, [H]);
filter_paths([Windoze = <<_, $:, _/binary>>|T], _Acc) ->
	filter_paths(T, [Windoze]);
%% add others
filter_paths([H|T], Acc) ->
	filter_paths(T, [H|Acc]);
filter_paths([], Acc) ->
	lists:reverse(Acc).

% current
make_path([<<$.>>|T], Acc) ->
	make_path(T, Acc);
% parent
make_path([<<"..">>|T], [_, <<$/>> | T0]) ->
	make_path(T, T0);
make_path([<<"..">>|T], Acc) ->
	make_path(T, Acc);
% windoze
make_path([H = <<_, $:>>|T], []) ->
	make_path(T, [text:to_upper(H)]);
%
make_path([H|T], Acc) when is_binary(H) ->
	make_path(T, [H, <<$/>> | Acc]);
%
make_path([], []) ->
	<<$/>>;
make_path([], Acc) ->
	list_to_binary(lists:reverse(Acc)).
	
%% TODO: test
code_path(Path) ->
	code_path(type(Path), Path).
%%
code_path(regular, Path) ->
	file_code_path(Path, extension(Path));
code_path(directory, Path) ->
	dir_code_path(Path, name(Path)).
	
dir_code_path(Path, ?BIN_DIR) ->
	Path;
dir_code_path(Path, _) ->
	BeamDir = new([Path, ?BIN_DIR]),
	case type(BeamDir) of
	directory -> 
		BeamDir;
	_ -> 
		undefined
	end.
	
file_code_path(Archive, <<"ez">>) -> 	
	Name = name(Archive),
	InternalPath = <<Name/binary, $/, ?BIN_DIR/binary, $/>>,
	{ok, Entries} = zip:list_dir(binary_to_list(Archive)),
	EntryNames = [X || #zip_file{name = X} <- Entries],
	case lists:member(binary_to_list(InternalPath), EntryNames) of
	true ->
		new([Archive, InternalPath]);
	false ->
		undefined
	end;
file_code_path(Path, _) ->
	Parent = parent(Path),
	case name(Parent) of
	?BIN_DIR ->
		Parent;
	_ -> 
		undefined
	end.
		
ensure(Path) ->
	Force = new([Path, <<"force">>]),
	filelib:ensure_dir(binary_to_list(Force)).
	
%%
name(Path) ->
	lists:last(text:split(Path, ?PATH_SEPARATOR)).
%%
basename(Path) ->
	File = name(Path),
	case extension(File) of
	<<>> -> 
		File;
	Ext ->
		text:strip(File, <<$., Ext/binary, $$>>)
	end.

%%
parent(Path) ->
	new([Path, <<"..">>]).
	
%%
appname(CodePath) ->
	case name(CodePath) of
	?BIN_DIR ->
		Name = name(parent(CodePath)),
		[AppName|_Version] = text:split(Name, <<$->>),
		binary_to_atom(AppName, utf8);
	_ ->
		undefined
	end.

%%
extension(Path) ->
	case text:split(Path, ?PATH_SEPARATOR) of
	[] -> 
		<<>>;
	Parts ->
		File = lists:last(Parts),
		case lists:reverse(text:split(File, <<"(\\.)">>)) of
		% NOTE: is this valid? i.e. are 'dot' files like .bashrc "hidden' or "anonymous" files?
		[<<$.>>, <<$.>>|_] ->
			<<>>;
		[Ext, <<$.>>|_] -> 
			Ext;
		_ -> 
			<<>>
		end
	end.

%%
%resource(File, Route) ->
%	Info = file_info(File),
%	Extension = extension(File),
%	#ifile{
%		route = Route,
%		file = File,
%		mimetype = ice_mime:lookup(Extension),
%		modified = Info#file_info.mtime,
%		size = Info#file_info.size,
%		bin = undefined
%	}.
%%
save(Path, Bin) when is_binary(Path) ->
	save(binary_to_list(Path), Bin);
save(Path, Bin) when is_binary(Bin) ->
	file:write_file(Path, Bin);
save(Path, Term) ->
	save(Path, text:format(Term)).
	
%%
%load(Resource = #ifile{}) ->
%	Bin = load(Resource#ifile.file),
%	Resource#ifile{bin=Bin};

%
load(Path) ->
	Path0 = new(Path),
	case type(Path0) of
	regular ->	
		{ok, Bin, _}  = erl_prim_loader:get_file(binary_to_list(Path0)),
		Bin;
	_ ->
		undefined
	end.
	
%%
eval(File) ->
	Path = new(File),
	case type(Path) of
	regular ->
		try
			{ok, Term} = file:consult(binary_to_list(Path)),
			Term
		catch
		%% C{error,{103,erl_parse,["syntax error before: ","'{'"]}}
		_:{badmatch, {error, {Line, _, Message}}} -> 
			{error, [{file, Path}, {line, Line}, {reason, lists:flatten(Message)}]};
		Error:Reason -> 
			{Error, Reason}
		end;
	_ ->
		{error, Path}
	end.

%%
list() ->
	list(<<".">>).

%%
list(Ext) when is_atom(Ext) ->
	list(<<".">>, Ext);
list(Path) when ?is_string(Path) ->
	list(new(Path));
list(Path) when is_binary(Path) ->
	case type(Path) of
	directory ->
		{ok, Files} = erl_prim_loader:list_dir(binary_to_list(Path)),
		lists:reverse([new([Path, X]) || X <- Files]); %, is_regular(X)];
	_ ->
		{error, not_directory}
	end.
%%
list(Path, Ext) when is_atom(Ext) ->
	E = atom_to_binary(Ext, utf8),
	Regex = <<".*\\.", E/binary, "$">>,
	list(Path, Regex);
list(Path, Regex) ->
	Files = list(Path),
	[X || X <- Files, text:matches(X, Regex)].

exists(Path) ->
	type(Path) =/= undefined.
		
type(File) ->
	case file_info(File) of
	#file_info{type = Type} ->
		Type;
	_ ->
		undefined
	end.

%% Temp...?
modified(File) ->
	case file_info(File) of
	#file_info{mtime = Modified} ->
		Modified;
	_ ->
		undefined
	end.
	
%%	
open(Path, Opts) ->
	file:open(binary_to_list(Path), Opts).


%%
close(Fd) ->
	file:close(Fd).
	
%%
file_info(File) when is_binary(File) ->
	file_info(binary_to_list(File));
%
file_info(File) ->
	case erl_prim_loader:read_file_info(File) of
	{ok, Info} -> 
		Info;
	_ -> 
		undefined
	end.

%% 
find(BasePath, Ext) when is_atom(Ext) ->
	E = atom_to_binary(Ext, utf8),
	find(BasePath, <<".*\\.", E/binary, "$">>);
find(BasePath, Regex) when is_binary(Regex) ->
	Path = new(BasePath),
	find_files(Path, Regex).
%
find_files(Path, Regex) ->
	Files = list(Path),
	Matches = [X || X <- Files, text:matches(X, Regex)],
	DeepMatches = [find_files(X, Regex) || X <- Files, type(X) =:= directory],
	lists:append(Matches, lists:flatten(DeepMatches)).

