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

-module(gx_event).

-include("gx.hrl").
-include("gx_wx.hrl").

-export([add_listener/1, remove_listener/1, bind/3, decode/2]).

%% TEMP
-export([wx_event_types/0]).

%%
add_listener(Gx = #g{}) ->
	gx_wx:call(Gx, ?wxEventListener_new_0, <<>>).

%%
remove_listener(Gx = #g{event_listener = #wx_ref{ref = Ref, type = wxeEvtListener}}) ->
	gx_wx:call(Gx, ?wxEventListener_Destroy_1, <<Ref:32/unsigned-native>>).

%%
bind(Gx = #g{}, Ref = #wx_ref{}, {Event, Data}) ->
	gx_wx:bind_event(Gx, Ref, Event, Data).
	
%%
decode(G = #g{}, #wx{id = _WxID, obj = Ref, userData = {context_menu, Name, MenuRef}, event = E}) ->
	%?TTY({wx_event, Wx}),
	Type = gx_cache:get_type(G, Ref),
	Data = wxevent(G, E),
	{#gx{id = Name, ref = Ref, type = Type, event = MenuRef, data = Data}, context_menu};
decode(G = #g{}, #wx{id = _WxID, obj = Ref, userData = {Name, Event, Handler}, event = E}) ->
	%?TTY({event, has_user_data}),
	Type = gx_cache:get_type(G, Ref),
	Data = wxevent(G, E),
	{#gx{id = Name, ref = Ref, type = Type, event = Event, data = Data}, Handler};
decode(G = #g{}, #wx{id = WxID, obj = Ref, event = E}) ->
	Type = gx_cache:get_type(G, Ref),
	Data = wxevent(G, E),
	case gx_cache:get_command(G, WxID) of
	{undefined, Event, Handler} ->
		%?TTY({event, command_name_undefined}),
		{#gx{id = {command, WxID}, ref = Ref, type = Type, event = Event, data = Data}, Handler};
	{Name, Event, Handler} ->
		%?TTY({event, command_full}),
		{#gx{id = Name, ref = Ref, type = Type, event = Event, data = Data}, Handler};
	undefined ->
		{Event, _, _, _} = Data,
		%?TTY({event, command_unknown_use_wx_type}),
		{#gx{id = gx_cache:lookup(G, Ref), ref = Ref, type = Type, event = Event, data = Data}, local}
	end.

%
wxevent(_, #wxCommand{type = Type, cmdString = S, commandInt = I, extraLong = L}) ->
	{Type, list_to_binary(S), I, L};
wxevent(_, #wxSpin{type = Type, commandInt = I}) ->
	[{wx_event, Type}, {value, I}];
wxevent(G, #wxTree{type = Type, item = I, itemOld = L, pointDrag = DragPoint}) ->
	case gx_cache:lookup(G, I) of
	_D = {{_ParentId, Data}, _TreeRef} ->
		%?TTY({lookup, I, D}),
		[{wx_event, Type}, {value, Data}, {item_id, I}, {previous_id, L}, {pos, DragPoint}];
	undefined ->
		[{wx_event, Type}, {item_id, I}, {previous_id, L}, {pos, DragPoint}]
	end;
wxevent(_, #wxDate{type = Type, date = Date}) ->
	{Type, Date, 0, 0};
wxevent(_, #wxMouse{type=Type, x = X, y = Y}) ->
	{Type, {X, Y}, 0, 0};
wxevent(_, E = #wxStyledText{type = Type, key = Key, modifiers = Mods}) ->
	{Type, Key, Mods, E};
wxevent(_, #wxFileDirPicker{type = command_dirpicker_changed, path = Path}) ->
	[{wx_event, command_dirpicker_changed}, {value, {directory, list_to_binary(Path)}}];
wxevent(_, #wxFileDirPicker{type = command_filepicker_changed, path = Path}) ->
	[{wx_event, command_filepicker_changed}, {value, {regular, list_to_binary(Path)}}];
wxevent(_, #wxColourPicker{type = Type, colour = Color}) ->
	[{wx_event, Type}, {value, {color, Color}}];
wxevent(_, {_RecName, Type}) ->
	{Type, <<>>, 0, 0}.

% Temporary export. This will be removed.
wx_event_types() -> [
	aui_find_manager, 
	aui_pane_button, 
	aui_pane_close, 
	aui_pane_maximize, 
	aui_pane_restore, 
	aui_render, 
	calendar_day_changed, 
	calendar_doubleclicked, 
	calendar_month_changed, 
	calendar_sel_changed, 
	calendar_weekday_clicked, 
	calendar_year_changed, 
	char, 
	char_hook, 
	child_focus, 
	close_window, 
	command_auinotebook_allow_dnd, 
	command_auinotebook_begin_drag, 
	command_auinotebook_bg_dclick, 
	command_auinotebook_button, 
	command_auinotebook_drag_done, 
	command_auinotebook_drag_motion, 
	command_auinotebook_end_drag, 
	command_auinotebook_page_changed, 
	command_auinotebook_page_changing, 
	command_auinotebook_page_close, 
	command_auinotebook_page_closed, 
	command_auinotebook_tab_middle_down, 
	command_auinotebook_tab_middle_up, 
	command_auinotebook_tab_right_down, 
	command_auinotebook_tab_right_up, 
	command_button_clicked, 
	command_checkbox_clicked, 
	command_checklistbox_toggled, 
	command_choice_selected, 
	command_colourpicker_changed, 
	command_combobox_selected, 
	command_dirpicker_changed, 
	command_enter, 
	command_filepicker_changed, 
	command_fontpicker_changed, 
	command_html_link_clicked, 
	command_kill_focus, 
	command_left_click, 
	command_left_dclick, 
	command_list_begin_drag, 
	command_list_begin_label_edit, 
	command_list_begin_rdrag, 
	command_list_cache_hint, 
	command_list_col_begin_drag, 
	command_list_col_click, 
	command_list_col_dragging, 
	command_list_col_end_drag, 
	command_list_col_right_click, 
	command_list_delete_all_items, 
	command_list_delete_item, 
	command_list_end_label_edit, 
	command_list_insert_item, 
	command_list_item_activated, 
	command_list_item_deselected, 
	command_list_item_focused, 
	command_list_item_middle_click, 
	command_list_item_right_click, 
	command_list_item_selected, 
	command_list_key_down, 
	command_listbox_doubleclicked, 
	command_listbox_selected, 
	command_menu_selected, 
	command_notebook_page_changed, 
	command_notebook_page_changing, 
	command_radiobox_selected, 
	command_radiobutton_selected, 
	command_right_click, 
	command_scrollbar_updated, 
	command_set_focus, 
	command_slider_updated, 
	command_spinctrl_updated, 
	command_splitter_doubleclicked, 
	command_splitter_sash_pos_changed, 
	command_splitter_sash_pos_changing, 
	command_splitter_unsplit, 
	command_text_enter, 
	command_text_updated, 
	command_togglebutton_clicked, 
	command_tool_enter, 
	command_tool_rclicked, 
	command_tree_begin_drag, 
	command_tree_begin_label_edit, 
	command_tree_begin_rdrag, 
	command_tree_delete_item, 
	command_tree_end_drag, 
	command_tree_end_label_edit, 
	command_tree_get_info, 
	command_tree_item_activated, 
	command_tree_item_collapsed, 
	command_tree_item_collapsing, 
	command_tree_item_expanded, 
	command_tree_item_expanding, 
	command_tree_item_gettooltip, 
	command_tree_item_menu, 
	command_tree_item_middle_click, 
	command_tree_item_right_click, 
	command_tree_key_down, 
	command_tree_sel_changed, 
	command_tree_sel_changing, 
	command_tree_set_info, 
	command_tree_state_image_click, 
	command_vlbox_selected, 
	context_menu, 
	create, 
	date_changed, 
	destroy, 
	detailed_help, 
	display_changed, 
	end_session, 
	enter_window, 
	erase_background, 
	grid_cell_begin_drag, 
	grid_cell_change, 
	grid_cell_left_click, 
	grid_cell_left_dclick, 
	grid_cell_right_click, 
	grid_cell_right_dclick, 
	grid_col_size, 
	grid_editor_created, 
	grid_editor_hidden, 
	grid_editor_shown, 
	grid_label_left_click, 
	grid_label_left_dclick, 
	grid_label_right_click, 
	grid_label_right_dclick, 
	grid_range_select, 
	grid_row_size, 
	grid_select_cell, 
	help, 
	iconize, 
	idle, 
	joy_button_down, 
	joy_button_up, 
	joy_move, 
	joy_zmove, 
	key_down, 
	key_up, 
	kill_focus, 
	leave_window, 
	left_dclick, 
	left_down, 
	left_up, 
	maximize, 
	menu_close, 
	menu_highlight, 
	menu_open, 
	middle_dclick, 
	middle_down, 
	middle_up, 
	motion,
	mouse_capture_changed, 
	mousewheel, 
	move, 
	navigation_key, 
	nc_enter_window, 
	nc_leave_window, 
	nc_left_dclick, 
	nc_left_down, 
	nc_left_up, 
	nc_middle_dclick, 
	nc_middle_down, 
	nc_middle_up, 
	nc_motion, 
	nc_paint, 
	nc_right_dclick, 
	nc_right_down, 
	nc_right_up, 
	paint, 
	paint_icon, 
	palette_changed, 
	query_end_session, 
	query_new_palette, 
	right_dclick, 
	right_down, 
	right_up, 
	sash_dragged, 
	scroll_bottom, 
	scroll_changed, 
	scroll_linedown, 
	scroll_lineup, 
	scroll_pagedown, 
	scroll_pageup, 
	scroll_thumbrelease, 
	scroll_thumbtrack, 
	scroll_top, 
	scrollwin_bottom, 
	scrollwin_linedown, 
	scrollwin_lineup, 
	scrollwin_pagedown, 
	scrollwin_pageup, 
	scrollwin_thumbrelease, 
	scrollwin_thumbtrack, 
	scrollwin_top, 
	set_cursor, 
	set_focus, 
	show, 
	size, 
	spin, 
	spin_down, 
	spin_up, 
	stc_autocomp_selection, 
	stc_calltip_click, 
	stc_change, 
	stc_charadded, 
	stc_do_drop, 
	stc_doubleclick, 
	stc_drag_over, 
	stc_dwellend, 
	stc_dwellstart, 
	stc_hotspot_click, 
	stc_hotspot_dclick, 
	stc_key, 
	stc_macrorecord, 
	stc_marginclick, 
	stc_modified, 
	stc_needshown, 
	stc_painted, 
	stc_romodifyattempt, 
	stc_savepointleft, 
	stc_savepointreached, 
	stc_start_drag, 
	stc_styleneeded, 
	stc_updateui, 
	stc_uridropped, 
	stc_userlistselection, 
	stc_zoom, 
	sys_colour_changed, 
	update_ui
].



