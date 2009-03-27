%%
%% GX Framework
%% Copyright 2009 <steven.charles.davis@gmail.com>. All rights reserved.
%% LICENSE: The correct license type has not yet been determined.
%%
-module(gx_util).
-version("0.2").
-author('steve@simulacity.com').

-include("../include/gx.hrl").
-include_lib("wx/include/wx.hrl").

-define(GX_APPLICATION, gx).
-define(GX_PATHS, gx_paths).

-compile(export_all).
-export([get_option/3, get_atom/3, get_boolean/3, 
	get_integer/3, get_string/3, get_resource/2]).

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
	case get(gx_icons) of
	undefined ->
		{ok, WxeIcon} = find_resource("wxe.xpm"),
		IconPath = filename:join(filename:dirname(WxeIcon), "icons"),
		{ok, Files} = file:list_dir(IconPath),
		IconList = [filename:join(IconPath, X) || X <- Files, filename:extension(X) == ".gif"],
		Icons = [{list_to_atom(filename:basename(Icon, ".gif")), 
			wxBitmap:new(Icon, [{type, ?wxBITMAP_TYPE_GIF}])} || Icon <- IconList],
		ImageList = wxImageList:new(16, 16), %, [{mask, false}, {initial_count, length(Icons)}]).
		IconMap = [{GxName, wxImageList:add(ImageList, WxIcon)} || {GxName, WxIcon} <- Icons],
		put(gx_iconmap, IconMap),
		put(gx_icons, ImageList),
		ImageList;
	Value ->
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
get_resource_paths() ->
	case get(gx_paths) of 
	undefined ->
		AppPaths =
		case application:get_application() of
		{ok, App} ->
			LibPath = code:lib_dir(App),
			case application:get_env(resources) of 
			{ok, Paths} -> 
				[LibPath | [filename:join([LibPath, P]) || P <- Paths]];
			undefined -> 
				[LibPath]
			end;
		undefined -> []
		end,
		GxPaths = 
		case application:get_env(?GX_APPLICATION, resources) of
		{ok, Paths2} ->
			GxLibPath = code:lib_dir(?GX_APPLICATION),
			[GxLibPath | [filename:join([GxLibPath, P]) || P <- Paths2]];
		undefined -> []
		end,
		{ok, WorkDir} = file:get_cwd(),
		%% TODO: This is ugly as hell but it works...
		AllPaths = lists:append([[WorkDir], AppPaths, GxPaths]),
		%% Nuke dupes
		UniquePaths = sets:to_list(sets:from_list(AllPaths)),
		undefined = put(gx_paths, lists:sort(UniquePaths)),
		get(gx_paths);
	Value -> Value
	end.

%
find_resource(File) ->
	find_file([filename:join(X, File) || X <- get_resource_paths()]).

%
find_file([H|T]) ->
	case filelib:is_regular(H) of
	true -> {ok, filename:absname(H)};
	false -> find_file(T)
	end;
%
find_file([]) ->
	{error, resource_missing}.
