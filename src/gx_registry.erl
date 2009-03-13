%%
%%
%%
-module(gx_registry).
-version("alpha").
-author('steve@simulacity.com').

-include("gx.hrl").
-include("gx_registry.hrl").
-include_lib("wx/include/wx.hrl").

-compile(export_all).
-export([start/0, stop/0, info/0]).
-export([get_option/3, get_atom/3, get_boolean/3, 
	get_integer/3, get_string/3, get_resource/2]).

%%
start() ->
	case erlang:system_info(smp_support) of 
	true -> 
		Root = wx:new(), % minimally
		
		%% TODO - For now, just get access to the environment
		%% for resource loading purposes..
		application:load(gx),
		
		case ets:info(?GX_COMPONENTS) of 
		undefined ->
			ets:new(?GX_COMPONENTS, [set, named_table, public]),
			ets:insert(?GX_COMPONENTS, {{self(), gx}, Root});
		_ -> ok
		end,
		case ets:info(?GX_COMMANDS) of 
		undefined ->
			ets:new(?GX_COMMANDS, [set, named_table, public]);
		_ -> ok
		end,
		Root;
	false ->
		{error, no_smp}
	end.

%%
stop() ->
	case ets:info(?GX_COMMANDS) of
	undefined -> ok;
	_ -> 
		ets:delete(?GX_COMMANDS)
	end,
	case ets:info(?GX_COMPONENTS) of
	undefined -> ok;
	_ -> 
		ets:delete(?GX_COMPONENTS)
	end,
	wx:destroy().

%%
info() ->
	case ets:info(?GX_COMPONENTS) of
	undefined -> no_gx;
	_ -> 
		Components = ets:tab2list(?GX_COMPONENTS),
		io:format("Registry: ~p records~n~p~n", [length(Components), Components]),
		case ets:info(?GX_COMMANDS) of
		undefined ->
			io:format("Mapping: <none>~n", []);
		_ -> 
			Commands = ets:tab2list(?GX_COMMANDS),
			io:format("Commands: ~p records~n~p~n", [length(Commands), Commands])
		end
	end.

%%
add_component(undefined, Component) ->
	Component;
add_component(GxName, Component) when is_atom(GxName) ->
	case ets:member(?GX_COMPONENTS, {self(), GxName}) of 
	true -> ignore;
	false -> ets:insert_new(?GX_COMPONENTS, {{self(), GxName}, Component})
	end,
	Component.

%%
remove_component(Component, Name) when is_tuple(Component) ->
	remove_component(Name, ?GX_COMMANDS),
	remove_component(Name, ?GX_COMPONENTS),
	wxWindow:destroy(Component);
remove_component(Name, DB) when is_atom(Name), is_atom(DB) ->	
	Process = self(),
	Delete = fun(X, Acc) ->
		case X of 
		{{Process, _}, _} -> ets:delete_object(DB, X), Acc + 1;
		_ -> Acc
		end
	end,
	Deleted = ets:foldr(Delete, 0, DB),
	io:format("[DESTROY ] ~p + ~p Refs~n", [{DB, Name}, Deleted]).

%%
lookup_component(Name) ->
	Process = self(),
	case ets:lookup(?GX_COMPONENTS, {Process, Name}) of 
	[{{Process, Name}, Component}] -> Component;
	_ -> undefined
	end.

%%
add_command_handler(GxName, GxHandler) ->	
	case ets:member(?GX_COMMANDS, {self(), GxName}) of 
	true -> 
		Command = -1, 
		ignored;
	false -> 
		Command = ?wxID_HIGHEST + 1001 + ets:info(?GX_COMMANDS, size),
		ets:insert_new(?GX_COMMANDS, {{self(), Command}, GxHandler})
	end,
	%io:format("[COMMAND] '~p' [~p] ~p~n", [CommandID, GxName, Handler]),
	Command.

%% TODO?: do we also need remove_command? it would be symmetrical...

%% TODO?: could do a consistency check on Type...
lookup_command_handler(Command) ->
	Process = self(),
	case ets:lookup(?GX_COMMANDS, {Process, Command}) of 
	[{{Process, Command}, {_Type, Handler}}] -> Handler;
	_ -> undefined
	end.

%%
%% Component attribute helper functions
%%

%%
get_option(Key, _, [{Key, Value}|_]) ->
	Value;
get_option(Key, Default, [_|T]) ->
	get_option(Key, Default, T);
get_option(_, Default, []) ->
	Default.

%%
get_atom(Key, Default, Opts) -> 
	case get_option(Key, Default, Opts) of 
	Value when is_atom(Value) -> Value;
	Value when is_list(Value) -> list_to_atom(Value)
	end.
	
get_boolean(Key, Default, Opts) ->
	case get_atom(Key, undefined, Opts) of
	true -> true;
	false -> false;
	_ -> Default
	end.
	
get_integer(Key, Default, Opts) ->
	case get_option(Key, Default, Opts) of 
	Value when is_integer(Value) -> Value;
	Value when is_list(Value) -> list_to_integer(Value)
	end.

get_string(Key, Default, Opts) -> 
	get_option(Key, Default, Opts).


%%
%% File system resource access
%%

%%
get_resource(icon, Opts) -> 
	{ok, Icon} = find_resource(get_option(icon, "wxe.xpm", Opts)),
	Type = icon_type(filename:extension(Icon)),
	wxIcon:new(Icon, [{type, Type}]).

%% Add more as necessary, not just because you can
icon_type(".xpm") -> ?wxBITMAP_TYPE_XPM;
icon_type(".png") -> ?wxBITMAP_TYPE_PNG;
icon_type(".bmp") -> ?wxBITMAP_TYPE_BMP;
icon_type(_)      -> ?wxBITMAP_TYPE_INVALID.


%% Directories to check, in order:
% ./<mypath>/<myfile>
% <myappdir>/<myrsrcpath>/<mypath>/<myfile> 
% TODO?: <myappdir>/<myrsrcpath>/*recursive/<myfile>
% <myappdir>/<mypath>/<myfile>
% <gxapp>/<gxrsrcpath>/<myfile>
find_resource(File) ->
	AppPaths = 
	case application:get_application() of
	{ok, App} ->
		LibPath = code:lib_dir(App),
		AppPath = filename:join(LibPath, File),
		case application:get_env(resources) of 
		{ok, Resources} -> 
			[filename:join([LibPath, Resources, File]), AppPath];
		undefined -> 
			[AppPath]
		end;
	undefined -> []
	end,
	GxPaths = 
	case application:get_env(gx, resources) of
	{ok, GxResources} -> 
		[filename:join([code:lib_dir(gx), GxResources, File])];
	undefined -> []
	end,
	Candidates = lists:append([[filename:absname(File)], AppPaths, GxPaths]),
	io:format("[RESOURCE] ~p~n", [Candidates]),
	find_file(Candidates).

find_file([H|T]) ->
	case filelib:is_regular(H) of
	true -> {ok, filename:absname(H)};
	false -> find_file(T)
	end;
find_file([]) ->
	{error, resource_missing}.

%%
%% GX Property to WX Method mapping
%%
map(wxFrame, status) -> setStatusText.
