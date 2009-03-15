%%
%% Internal: GX Implementation Headers
%%

% ETS Table Names
-define(GX_COMPONENTS, gx_components).
-define(GX_COMMANDS, gx_commands).

%
% Events
%
-define(GX_WINDOW_EVENTS, {wxWindow, [{onunload, close_window}]}).

-define(GX_MENU_EVENTS, {wxMenu, []}).

-define(GX_BUTTON_EVENTS, {wxButton, [{onclick, command_button_clicked}]}).

%
% Commands
%
-define(GX_MENU_COMMAND, {wxMenu, [
	{onselect, command_menu_selected}, 
	{oncheck, command_menu_selected}
]}).
