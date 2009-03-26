%%
%% GX Framework
%% Copyright 2009 <steven.charles.davis@gmail.com>. All rights reserved.
%% LICENSE: The correct license type has not yet been determined.
%%
-module(gx_registry).
-version("alpha").
-author('steve@simulacity.com').

-include("../include/gx.hrl").
-include_lib("wx/include/wx.hrl").

-define(GX_APPLICATION, gx).
% ETS Table Names
-define(GX_REGISTRY, gx_registry).
-define(GX_COMPONENTS, gx_components).
-define(GX_COMMANDS, gx_commands).

-compile(export_all).
-export([start/0, stop/0, info/0]).
-export([get_option/3, get_atom/3, get_boolean/3, 
	get_integer/3, get_string/3, get_resource/2]).

%% gx_registry
%% TODO: Replace error_logger when GX has its own gen_event log

%%
start() ->
	case erlang:system_info(smp_support) of 
	true -> 
		Root = wx:new(), % minimally
		
		%% TODO - For now, just get access to the environment
		%% for resource loading purposes..
		case application:get_application(?GX_APPLICATION) of
		{ok, ?GX_APPLICATION} -> ok;
		undefined -> application:load(?GX_APPLICATION)
		end,
		case ets:info(?GX_REGISTRY) of 
		undefined ->
			ets:new(?GX_REGISTRY, [set, named_table, public]),
			ets:insert(?GX_REGISTRY, {{self(), gx}, Root});
		_ -> ok
		end,
		case ets:info(?GX_COMPONENTS) of 
		undefined ->
			ets:new(?GX_COMPONENTS, [set, named_table, public]);
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
	_ -> ets:delete(?GX_COMMANDS)
	end,
	case ets:info(?GX_COMPONENTS) of
	undefined -> ok;
	_ -> ets:delete(?GX_COMPONENTS)
	end,
	case ets:info(?GX_REGISTRY) of
	undefined -> ok;
	_ -> ets:delete(?GX_REGISTRY)
	end,
	try
		wx:destroy()
	catch
		_:_ -> {ok, already_stopped}
	end.

%% 
info() ->
	case ets:info(?GX_REGISTRY) of
	undefined -> no_gx;
	_ -> 
		Registry = ets:tab2list(?GX_REGISTRY),
		Components = ets:tab2list(?GX_COMPONENTS),
		Commands = ets:tab2list(?GX_COMMANDS),
		{gx_registry, [
			{registry, length(Registry), Registry}, 
			{components, length(Components), Components}, 
			{commands, length(Commands), Commands}]}
	end.


%
report_info(GxName, Component) ->
%	error_logger:info_report([{registered_event, WxEvent}, {id, GxName}, {component, Component}, {handler, GxHandler}]),
	error_logger:info_report([
		{process, self()}, 
		{created, GxName}, 
		{component, Component}, 
		{resource_paths, find_resource("")}]).

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
	remove_component(Name, ?GX_COMPONENTS);
remove_component(Name, DB) when is_atom(Name), is_atom(DB) ->	
	Process = self(),
	Delete = fun(X, Acc) ->
		case X of 
		{{Process, _}, _} -> ets:delete_object(DB, X), Acc + 1;
		_ -> Acc
		end
	end,
	_Deleted = ets:foldr(Delete, 0, DB).
	% error_logger:info_report([{DB, destroy}, {Name, Deleted}]).

%%
lookup_component(Name) ->
	Process = self(),
	case ets:lookup(?GX_COMPONENTS, {Process, Name}) of 
	[{{Process, Name}, Component}] -> Component;
	_ -> undefined
	end.

%%
add_command(GxName, GxCallback) ->	
	case ets:member(?GX_COMMANDS, {self(), GxName}) of 
	true -> 
		Command = -1, 
		ignored;
	false -> 
		Command = ?wxID_HIGHEST + 1001 + ets:info(?GX_COMMANDS, size),
		put(Command, {GxName, GxCallback}),
		ets:insert_new(?GX_COMMANDS, {{self(), Command}, {GxName, GxCallback}})
	end,
	% error_logger:info_report([{?GX_COMMANDS, add}, {GxName, GxCallback}]),
	Command.

%% TODO?: could do a consistency check on Type...
lookup_command(Command) ->
	Process = self(),
	case ets:lookup(?GX_COMMANDS, {Process, Command}) of 
	[{{Process, Command}, {GxName, {GxEvent, _Type, Callback}}}] -> {GxName, GxEvent, Callback},
	{Name, {Event, WxType, GxCallback}} = get(Command),
	{Name, Event, GxCallback};
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

%% TODO: Change this as its'a complete hack!!
load_icons() ->
	case lookup_component(gx_icons) of
	undefined ->
		{ok, WxeIcon} = find_resource("wxe.xpm"),
		IconPath = filename:join(filename:dirname(WxeIcon), "icons"),
		{ok, Files} = file:list_dir(IconPath),
		IconList = [filename:join(IconPath, X) || X <- Files, filename:extension(X) == ".gif"],
		Icons = [{list_to_atom(filename:basename(Icon, ".gif")), 
			wxBitmap:new(Icon, [{type, ?wxBITMAP_TYPE_GIF}])} || Icon <- IconList],
		ImageList = wxImageList:new(16, 16), %, [{mask, false}, {initial_count, length(Icons)}]).
		IconMap = [{GxName, wxImageList:add(ImageList, WxIcon)} || {GxName, WxIcon} <- Icons],
		%% TODO: save the iconmap to ets!
		add_component(gx_iconmap, IconMap), %% TODO: does this really belong in the component registry?
		add_component(gx_icons, ImageList);
	Value ->
		io:format("RSRC ICONS ALREADY LOADED", []),
		Value
	end.

%%
get_resource(image, Opts) -> 
	{ok, Image} = find_resource(get_option(image, "gx.png", Opts)),
	Type = image_type(filename:extension(Image)),
	wxBitmap:new(Image, [{type, Type}]);
%%
get_resource(icon, Opts) -> 
	{ok, Icon} = find_resource(get_option(icon, "wxe.xpm", Opts)),
	Type = image_type(filename:extension(Icon)),
	wxIcon:new(Icon, [{type, Type}]).

%% Add more as necessary, not just because you can
image_type(".xpm") -> ?wxBITMAP_TYPE_XPM;
image_type(".png") -> ?wxBITMAP_TYPE_PNG;
image_type(".gif") -> ?wxBITMAP_TYPE_GIF;
image_type(".jpg") -> ?wxBITMAP_TYPE_JPEG;
image_type(".bmp") -> ?wxBITMAP_TYPE_BMP;
image_type(_)      -> ?wxBITMAP_TYPE_INVALID.


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
	case File of %% TODO: something of a hack here to report what paths are going to be searched
	"" -> Candidates;
	_ -> find_file(Candidates)
	end.
	
find_file([H|T]) ->
	case filelib:is_regular(H) of
	true -> {ok, filename:absname(H)};
	false -> find_file(T)
	end;
find_file([]) ->
	{error, resource_missing}.
