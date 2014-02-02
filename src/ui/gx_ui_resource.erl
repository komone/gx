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

-module(gx_ui_resource).

-include("gx.hrl").
-include("gx_wx.hrl").

-export([load_resources/2, append_resources/3, load_bitmap/2, load_icon/2]).

-compile(export_all).

%%
load_resources(Gx, Paths) ->
	Bitmaps = [load_bitmap(Gx, X) || X <- Paths],
	Ref = create_image_list(Gx, Bitmaps),
	{ok, Ref}.
%%
append_resources(Gx, Ref = #wx_ref{type = wxImageList}, Paths) ->
	Bitmaps = [load_bitmap(Gx, X) || X <- Paths],
	Ref = add_to_image_list(Gx, Ref, Bitmaps),
	{ok, Ref}.
	
append_resource(Gx, Ref = #wx_ref{type = wxImageList}, Bitmap = #wx_ref{type = wxBitmap}) ->
	_Index = gx_wx:call(Gx, ?wxImageList_Add_1, [Ref, Bitmap]).

%%
load_bitmap(Gx, Path) ->
	case path:type(Path) of
	regular ->
		Type = image_type(path:extension(Path)),
		gx_wx:call(Gx, ?wxBitmap_new_2_0, [Path, {options, [type], [{type, Type}]}]);
	_ ->
		undefined
	end.
	
%%
load_icon(Gx, Path) ->
	case path:type(Path) of
	regular ->
		Type = image_type(path:extension(Path)),
		gx_wx:call(Gx, ?wxIcon_new_2, [Path, {options, [type, desiredWidth, desiredHeight], [{type, Type}]}]);
	_ ->
		undefined
	end.

size(Gx, Ref = #wx_ref{type = wxImageList}) ->
	gx_wx:call(Gx, ?wxImageList_GetImageCount, [Ref]).

%% 
create_image_list(Gx = #g{}) ->
	create_image_list(Gx, 16, 16, []).
%%
create_image_list(Gx, Bitmaps) when is_list(Bitmaps) ->
	create_image_list(Gx, 16, 16, Bitmaps).
%%
create_image_list(Gx, W, H) when is_integer(W), is_integer(H) ->
	create_image_list(Gx, W, H, []).
%%
create_image_list(Gx, W, H, Bitmaps) ->
	Ref = gx_wx:call(Gx, ?wxImageList_new_3, [{W, H}, {options, [mask, initialCount], [{initialCount, 64}]}]),
	build_image_list(Gx, Ref, Bitmaps, 0).

%%
add_to_image_list(Gx, Ref = #wx_ref{type = wxImageList}, Bitmaps) when is_list(Bitmaps) ->
	Count = size(Gx, Ref),
	build_image_list(Gx, Ref, Bitmaps, Count).
	
%
build_image_list(Gx, Ref, [H = #wx_ref{type = wxBitmap}|T], Count) ->
	Count = gx_wx:call(Gx, ?wxImageList_Add_1, [Ref, H]),
	build_image_list(Gx, Ref, T, Count + 1);
build_image_list(_Gx, Ref, [], _) ->
	Ref.

%%
get_bitmap(Gx, Ref = #wx_ref{type = wxImageList}, Index) when is_integer(Index) ->
	gx_wx:call(Gx, ?wxImageList_GetBitmap, [Ref, Index]).
	
%%
get_icon(Gx, Ref = #wx_ref{type = wxImageList}, Index) when is_integer(Index) ->
	gx_wx:call(Gx, ?wxImageList_GetIcon, [Ref, Index]).

%
image_type(<<"xpm">>) -> ?wxBITMAP_TYPE_XPM;
image_type(<<"png">>) -> ?wxBITMAP_TYPE_PNG;
image_type(<<"gif">>) -> ?wxBITMAP_TYPE_GIF;
image_type(<<"jpg">>) -> ?wxBITMAP_TYPE_JPEG;
image_type(<<"bmp">>) -> ?wxBITMAP_TYPE_BMP;
image_type(_)         -> ?wxBITMAP_TYPE_INVALID.
