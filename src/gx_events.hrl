%%
%% Internal: GX Implementation Headers
%%

% ETS Table Names
-define(GX_COMPONENTS, gx_components).
-define(GX_COMMANDS, gx_commands).

%
% Events
%
%%%TODO - ALL THESE DEFINITIONS ARE INCOMPLETE!
-define(GX_WINDOW_EVENTS, {wxWindow, [{onunload, close_window}]}).

-define(GX_BUTTON_EVENTS, {wxButton, [{onclick, command_button_clicked}]}).
-define(GX_CHECKBOX_EVENTS, {wxCheckBox, [{onselect, command_checkbox_clicked}]}).
-define(GX_RADIOBUTTON_EVENTS, {wxRadioButton, [{onselect, command_radiobutton_selected}]}).
-define(GX_RADIOBOX_EVENTS, {wxRadioBox, [{onselect, command_radiobox_selected}]}).
-define(GX_LISTBOX_EVENTS, {wxListBox, [{onselect, command_listbox_selected}]}).

%% this *should* but does not work!
% WX BUG -define(GX_CHECKLISTBOX_EVENTS, {wxCheckListBox, [{onselect, command_checklistbox_toggled}]}).
-define(GX_CHECKLISTBOX_EVENTS, {wxCheckListBox, [{onselect, command_listbox_selected}]}).

-define(GX_CHOICE_EVENTS, {wxChoice, [{onselect, command_choice_selected}]}).
-define(GX_COMBOBOX_EVENTS, {wxComboBox, [{onselect, command_combobox_selected}, {onchange, command_text_updated}]}).
-define(GX_SLIDER_EVENTS, {wxSlider, [{onchange, command_slider_updated}]}).
% WX BUG -define(GX_SPINNER_EVENTS, {wxSpinCtrl, [{onchange, evt_spinctrl}]}).
-define(GX_SPINNER_EVENTS, {wxSpinCtrl, [{onchange, command_text_updated}]}).

-define(GX_MENU_EVENTS, {wxMenu, []}).
-define(GX_GRID_EVENTS, {wxGrid, []}).

%
% Commands
%
-define(GX_MENU_COMMAND, {wxMenu, [{onselect, command_menu_selected}]}).
