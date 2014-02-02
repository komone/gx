%% Copyright 2010-2011 Steve Davis <steve@simulacity.com>
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

-module(gx_ui_sizer).

-include("gx.hrl").
-include("gx_wx.hrl").

-export([add/4, create/4, create/5, create_box/5, get_sizer_flags/5]).

% Temp?
-export([layout/7, show/3]).

add(G, Sizer, Control, Options) ->
	gx_wx:call(G, ?wxSizer_Add_2_1, [Sizer, Control, 
		{options, [proportion, flag, border, userData], Options}]).

%add_sizer_to_sizer(#g{port = Port}, Sizer, Control, Options) ->
%	Args = [Sizer, Control, {options, [proportion, flag, border, userData], Options}],
%	gx_wx:call(Port, ?wxSizer_Add_2_0, Args).

%%
create(G, Ref, Layout, Size) ->
	OrientationFlag = get_orientation_flag(Layout),
	Sizer = gx_wx:call(G, ?wxBoxSizer_new, [OrientationFlag]),
	gx_wx:cast(G, ?wxSizer_SetMinSize_1, [Sizer, Size]),
	gx_wx:cast(G, ?wxWindow_SetSizer, [Ref, Sizer, {options, [deleteOld], []}]),
	Sizer.
%%
create(G, Ref, grid, Size, {Columns, Rows}) when Columns >= 1, Rows >= 1 ->
	Sizer = gx_wx:call(G, ?wxFlexGridSizer_new_4, [{Rows, Columns}, {0, 0}]),
	gx_wx:cast(G, ?wxFlexGridSizer_AddGrowableCol, [Sizer, 1, {options, [proportion], []}]),
	gx_wx:cast(G, ?wxSizer_SetMinSize_1, [Sizer, Size]),
	gx_wx:cast(G, ?wxWindow_SetSizer, [Ref, Sizer, {options, [deleteOld], []}]),
	Sizer;
%% used by panel -- is this bogus?
create(G, Ref, Layout, Size, _) ->
	create(G, Ref, Layout, Size).
%% NOTE: what about GridSizer? original impl for panel had...
%get_sizer(grid, Columns, Rows) when Columns =< 0, Rows > 0 ->
%	wxGridSizer:new(Rows, 2, 0, 0); %Padding, Padding);
	
% IMPL: wxStaticBox must be added as a first sibling in the sizer, here we use the alternate
% creation method which does this for us.
create_box(G, Ref, Layout, Size, undefined) ->
	create_box(G, Ref, Layout, Size, <<>>);
create_box(G, Ref, Layout, Size, Label) ->
	Orientation = get_orientation_flag(Layout),
	Sizer = gx_wx:call(G, ?wxStaticBoxSizer_new_3,  [Orientation, Ref, {options, [label], [{label, Label}]}]),
	gx_wx:cast(G, ?wxSizer_SetMinSize_1, [Sizer, Size]),
	gx_wx:cast(G, ?wxWindow_SetSizer, [Ref, Sizer, {options, [deleteOld], []}]),
	Sizer.

show(Gx, Ref, Value) ->
	Parent = gx_cache:lookup(Gx, Ref),
	Sizer = gx_wx:call(Gx, ?wxWindow_GetSizer, [Parent, Ref]),
	gx_wx:call(Gx, ?wxSizer_Show_2_2, [Sizer, Ref, {options, [show, recursive], [{show, Value}]}]).
	

%% 
layout(G, Parent, Ref, Sizer, Align, Fill, Border) ->
	ParentSizer = gx_wx:call(G, ?wxWindow_GetSizer, [Parent]),
	SizerOptions = get_sizer_flags(G, ParentSizer, Align, Fill, Border),
	add(G, ParentSizer, Ref, SizerOptions),	
	% IMPL: Important - layout doesn't work without calling sizer:fit after
	% adding the children. Ensures component sizes are correctly propagated
	% from child->parent
	gx_wx:call(G, ?wxSizer_Fit, [Sizer, Parent]).

%%
get_sizer_flags(G, Sizer, Align, Fill, Border) ->
	Orientation = get_orientation(G, Sizer), 
	Proportion = get_proportion(Fill, Orientation),
	ExpandFlag = get_expand_flag(Fill, Orientation),
	AlignmentFlag = get_alignment_flag(Align, Orientation),
	[{proportion, Proportion}, {flag, ?wxALL bor ExpandFlag bor AlignmentFlag}, {border, Border}].
	
%%
get_orientation(_, #wx_ref{ref = 0}) ->
	vertical;
get_orientation(G, Sizer) ->
	Orientation = gx_wx:call(G, ?wxBoxSizer_GetOrientation, [Sizer#wx_ref{type = wxBoxSizer}]),
	case Orientation of
	?wxHORIZONTAL -> % 4
		horizontal;
	?wxVERTICAL -> % 8
		vertical;
	_ -> % flexgridsizer, etc
		vertical
	end.

%%
get_orientation_flag(vertical) ->
	?wxVERTICAL;
get_orientation_flag(column) ->
	?wxVERTICAL;
get_orientation_flag(horizontal) ->
	?wxHORIZONTAL;
get_orientation_flag(row) ->
	?wxHORIZONTAL;
get_orientation_flag(_) ->
	?wxVERTICAL.
%
get_proportion(width, horizontal) ->
	1;
get_proportion(height, vertical) ->
	1;
get_proportion(both, _) ->
	1;
get_proportion(true, _) ->
	1;
get_proportion(_, _) ->
	0.
%	
get_expand_flag(width, vertical) ->
	?wxEXPAND;
get_expand_flag(height, horizontal) ->
	?wxEXPAND;
get_expand_flag(both, _) ->
	?wxEXPAND;
get_expand_flag(true, _) ->
	?wxEXPAND;
get_expand_flag(_, _) ->
	0.
%
get_alignment_flag(left, vertical) ->
	?wxALIGN_LEFT;
get_alignment_flag(left, horizontal) ->
	?wxALIGN_TOP;
get_alignment_flag(right, vertical) ->
	?wxALIGN_RIGHT;
get_alignment_flag(right, horizontal) ->
	?wxALIGN_RIGHT; %?wxALIGN_BOTTOM;
get_alignment_flag(center, _) ->
	?wxALIGN_CENTER.

