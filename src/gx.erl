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

-module(gx).

-include("gx.hrl").
-include("gx_wx.hrl").

-export([info/0, info/1, info/2]).
-export([load/1, load/2, compile/1]).
-export([start/0, start/1, stop/0, stop/1]).
-export([batch/1, create/1, create/2, read/2, config/1, config/2, config/3, destroy/1]).
-export([context/0, context/1, names/0, find/1, parent/1, ide/0]).

%% should we include convenience functions a la GS?
%% -export([window/1, splashscreen/1, panel/2, ...]).
-export([dialog/2, dialog/3]).

info() -> 
	case context() of
	undefined ->
		not_started;
	Gx = #g{} -> 
		Info = [
			% {version, ?GX_VERSION},
			{wx_version, gx_driver:version()}, 
			{types, lists:sort([X || {X, _} <- gx_cache:types(Gx)])},
			{resources, gx_cache:paths(Gx)}
		],
		io:format(user, "~p~n", [Info])
	end.

info(names) ->
	gx_cache:names(context());
info(types) ->
	case context() of
	undefined ->
		not_started;
	Gx = #g{} ->
		List = [X || {_, X} <- gx_cache:types(Gx)],
		Modules = sets:to_list(sets:from_list(List)),
		GxWx = lists:flatten([Module:mapping_info() || Module <- Modules]),
		Info = lists:sort([{Type, Attrs} || #gx_wx{type = Type, attributes = Attrs} <- GxWx]),
		io:format(user, "~p~n", [Info])
	end;
info(Type) when is_atom(Type) ->
	case context() of
	undefined ->
		not_started;
	Gx = #g{} ->
		case gx_cache:get_module(Gx, Type) of
		undefined ->
			undefined;
		Module ->
			[Info] = [{Type0, Attrs} || #gx_wx{type = Type0, attributes = Attrs} <- Module:mapping_info(), Type0 =:= Type],
			Info
		end
	end.

info(wx, Type) ->
	case context() of
	undefined ->
		not_started;
	Gx = #g{} ->
		case gx_cache:get_module(Gx, Type) of
		undefined ->
			undefined;
		Module ->
			[Info] = [Wx || Wx = #gx_wx{type = Type0} <- Module:mapping_info(), Type0 =:= Type],
			Info
		end
	end.

%% Core GUI startup
%%
load(X) when is_atom(X) ->
	Name = atom_to_binary(X, utf8),
	load(path:new([<<Name/binary, ".gui">>]));
load(File) ->
	load(undefined, File).
%%
load(ID, File) when is_atom(ID) ->
	case path:type(File) of
	regular ->
		case path:extension(File) of
		<<"gui">> ->
			Bin = path:load(File),
			Term = binary_to_term(Bin); % binary_to_term(Bin, [safe]);
		<<"xml">> ->
			Term = gx_xml:load(File)
		end,
		GUI = update(ID, File, Term),
		start(GUI);
	Error ->
		{error, Error}
	end.

compile(Gxml) ->
	Term = gx_xml:load(Gxml),
	Filename = path:name(Gxml),
	GxFile = path:new(<<Filename/binary, ".gui">>),
	Bin = term_to_binary(Term, [{compressed, 6}, {minor_version, 1}]),
	path:save(GxFile, Bin),
	{ok, GxFile}.
%%
start() ->
	start(?MODULE).
%%
start(Name) when is_atom(Name) ->
	start(#gui{id = Name, module = gx_runner, def = []});
start(GUI = #gui{}) ->
	gx_srv:start(GUI).

%%
update(ID, File, UI) ->
	UI0 = update_id(ID, UI),
	update_path(File, UI0).
	
update_id(undefined, UI = #gui{id = undefined}) ->
	UI#gui{id = ?MODULE};
update_id(undefined, UI) ->
	UI;
update_id(ID, UI) ->
	UI#gui{id = ID}.
	
update_path(_File, UI = #gui{path = undefined}) ->
	UI;
update_path(File, UI = #gui{path = Path}) ->
	Path0 = path:new([path:parent(File), Path]),
	directory = path:type(Path0),
	UI#gui{path = Path0}.

%%
stop() ->
	stop(?MODULE).
%%
stop(Name) when is_atom(Name) ->
    gx_srv:stop(Name).


batch(F) when is_function(F) ->
	gx_wx:batch(context(), F).

%%
create(Def) when is_tuple(Def) ->
	create([Def]);
create(Defs) when is_list(Defs) ->
	gx_ui:create_tree(context(), gx_wx:null(), Defs).

create(Parent, Def) ->
	Ref = gx_cache:lookup(context(), Parent),
	gx_ui:create(context(), Ref, [Def]).

%% NOTE: Diverges from GS as there are extended use cases 
%% for example, gx:read(listbox, {selected, [{item, 0}]})?
read(Name, Property) when is_atom(Property) ->
	gx_ui:read(context(), Name, Property);
read(Name, Properties) when is_list(Properties) ->
	[{X, read(Name, X)} || X <- Properties].

%% NOTE: Diverges from GS as there are extended use cases 
%% gx:config(listbox, selected, {{item, 0}, {style, blah}})?
config(Name, Property, Value) ->
	config(Name, [{Property, Value}]).
%% NOTE: This may not always be the desired effect, and masks the
% wxWindow:update function
config(Name, update) ->
	config(Name, [{layout, true}, {refresh, true}]);
config(Name, Property) when is_atom(Property) ->
	config(Name, [{Property, true}]);
config(Name, Properties) when is_list(Properties) ->
	gx_ui:config(context(), Name, Properties).

config(List) ->
	G = context(),
	List0 = lists:flatten([normalize(G, Name, Properties) || {Name, Properties} <- List]),
	?TTY(List0),
	gx_ui:config(G, List0).

normalize(G, Name, List) ->
	{ok, Module, Ref} = gx_cache:lookup_module(G, Name),
	F = fun(X) when is_atom(X) -> {Module, Ref, X, true}; ({K, V}) -> {Module, Ref, K, V} end,
	lists:map(F, List).

destroy(Name) ->
	gx_ui:destroy(context(), Name).

dialog(Parent, Type) ->
	dialog(Parent, Type, []).

dialog(Parent, Type, Opts) when is_atom(Parent) ->
	Ref = gx_cache:lookup(context(), Parent),
	dialog(Ref, Type, Opts);
dialog(Parent, message, Opts) ->
	gx_ui_dialog:message(context(), Parent, Opts);
dialog(Parent, entry, Opts) ->
	gx_ui_dialog:text_entry(context(), Parent, Opts);	
dialog(Parent, directory, Opts) ->
	gx_ui_dialog:directory_chooser(context(), Parent, Opts);
dialog(Parent, file, Opts) ->
	gx_ui_dialog:file_chooser(context(), Parent, Opts);
dialog(Parent, font, Opts) ->
	gx_ui_dialog:font_chooser(context(), Parent, Opts);
dialog(Parent, color, Opts) ->
	gx_ui_dialog:color_chooser(context(), Parent, Opts).

%% REPL usage 
context() ->
	gx_srv:context().
	
%% REPL use to change between various running UI servers
context(Name) when is_atom(Name) ->
	gx_srv:context(Name).
	
%% REPL get the currently registered names 
names() ->
	{ok, KV} = gx_cache:names(context()),
	Keys = [K || {K, _} <- KV],
	lists:sort(Keys).

%% REPL get a reference from a name
find(Name) when is_atom(Name) ->
	gx_cache:lookup(context(), Name).

parent(Ref = #wx_ref{}) ->
	gx_cache:lookup(context(), Ref).

ide() ->
	Path = path:new([code:priv_dir(gx), <<"ide.xml">>]),
	load(Path).
