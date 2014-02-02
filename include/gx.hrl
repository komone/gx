%% Copyright 2010-2014 Steve Davis <steve@simulacity.com>
%
% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at:
% 
% http://www.apache.org/licenses/LICENSE-2.0
% 
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT 
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the 
% License for the specific language governing permissions and limitations under
% the License.

-define(VERSION, {0, 4, 0}).

-vsn(?VERSION).
-author('steve@simulacity.com').

%-define(DEBUG, true).

%-ifdef(DEBUG).
  -define(TTY(Term), io:format(user, "[~p ~p] ~p~n", [self(), ?MODULE, Term])).
%-else.
%  -define(TTY(Term), ok).
%-endif.

% GX Graphic Context
-record(g, {server, port, event_listener, cache, name}).

% GX UI Definition
-record(gui, {id, module, def = [], path}). 

% GX Event Record
-record(gx, {id, ref, type, event, data = {undefined, <<>>, 0, 0}, user = []}).

% The Original GS Event Record
%% THIS WILL BE REMOVED - REFERENCE PURPOSES ONLY
% {gs, IdOrName, EventType, Data, Args}
-record(gs, {
	id = undefined, % IdOrName
	event = undefined, % see GS_EVENTS 
	data = [], % user defined data
	args = [] % event attributes
}).

% Components
-define(UNTITLED, <<"Untitled">>).

%% The Original built-in GS Object names. 
%% THIS WILL BE REMOVED - REFERENCE PURPOSES ONLY
-define(GS_OBJECTS, [
	arc, button, canvas, editor, entry, frame, grid, gridline, 
	image, label, line, listbox, menu, menubar, menubutton, 
	menuitem, oval, polygon, rectangle, scale, text, window
]).

-define(BASE_ATTRS, id, ref, label).

% Top-Level Windows
-define(FRAME_ATTRS, ?BASE_ATTRS, pos = {-1, -1}, size = {-1, -1}, callbacks = [], icon, title = ?UNTITLED, show = true).
-record(frame, {?FRAME_ATTRS, virtual = false, content = []}).
-record(dialog, {?FRAME_ATTRS, content = []}).
-record(aui, {?FRAME_ATTRS, content = []}).

% Furniture
-record(menubar, {?BASE_ATTRS, content = []}).
-record(menu, {?BASE_ATTRS, icon, content = []}).
-record(menuitem, {?BASE_ATTRS, type, icon, enabled = true, checked = false, callbacks = []}).
-record(toolbar, {?BASE_ATTRS, content = []}).
-record(toolbutton, {?BASE_ATTRS, icon, callbacks = []}).
-record(statusbar, {?BASE_ATTRS, message}).
-record(separator, {}).

% Containers
-define(CONTAINER_ATTRS, ?BASE_ATTRS, size = {-1, -1}, align = left, border = 0, fill = both, content = []).
-record(panel, {?CONTAINER_ATTRS, layout = column, rows, cols, color, background}).
-record(box, {?CONTAINER_ATTRS, layout = column, color}).
-record(splitpane, {?CONTAINER_ATTRS, layout = vertical, sash = 0, gravity = 0.5}).
-record(grid, {?CONTAINER_ATTRS, cols, rows}).

% Controls
-define(CONTROL_ATTRS, ?BASE_ATTRS, size = {-1, -1}, align = left, border = 0, fill = none, enable = true, callbacks = []).
-define(LIST_CONTROL_ATTRS, ?CONTROL_ATTRS, choices = [], selected). 
-define(RANGE_CONTROL_ATTRS, ?CONTROL_ATTRS, min, max, value). 

%% NOTE: used internally maybe move to gx_wx.hrl?
-record(control, {?CONTROL_ATTRS}).

% Static Controls
-record(line, {?CONTROL_ATTRS, orientation}).
-record(text, {?CONTROL_ATTRS, justify, wrap, fixed, font}).
-record(image, {?CONTROL_ATTRS, path}).

% Control widgets
-record(button, {?CONTROL_ATTRS, icon}).
-record(bitmapbutton, {?CONTROL_ATTRS, icon}).
-record(checkbox, {?CONTROL_ATTRS, justify, value = false}).
-record(togglebutton, {?CONTROL_ATTRS, icon}).
-record(radiobutton, {?CONTROL_ATTRS}).
-record(progressbar, {?CONTROL_ATTRS, layout, smooth, percent}).

-record(checklist, {?LIST_CONTROL_ATTRS}).
-record(choice, {?LIST_CONTROL_ATTRS}).
-record(combo, {?LIST_CONTROL_ATTRS}).
-record(list, {?LIST_CONTROL_ATTRS}).
-record(radiobox, {?LIST_CONTROL_ATTRS}).

-record(date, {?RANGE_CONTROL_ATTRS}).
-record(slider, {?RANGE_CONTROL_ATTRS, ticks, labels}).
-record(spinner, {?RANGE_CONTROL_ATTRS, wrap = true}).

-record(picker, {?CONTROL_ATTRS, type, value}).

% Composite controls
-record(tabs, {?CONTROL_ATTRS, icon, content = []}).
-record(tab, {?CONTROL_ATTRS, icon, content = []}).
-record(tree, {?CONTROL_ATTRS, icon, content = [], data}).
-record(treeitem, {?BASE_ATTRS, icon, content = [], data}).
-record(treebook, {?CONTROL_ATTRS, content = []}).
-record(calendar, {?CONTROL_ATTRS}).
-record(input, {?CONTROL_ATTRS, style, enabled, src, content}).
-record(editor, {?CONTROL_ATTRS, lexer = none}).

% Custom controls
-record(filetree, {?CONTROL_ATTRS, path}).

%% The Original GS font record. 
%% THIS WILL BE REMOVED - REFERENCE PURPOSES ONLY
-record(gs_font, {family, style, size}).

% GX Font Record
-record(font, {
	id = undefined,
	family = default,
	face = undefined,
	size = undefined,
	weight = undefined, 
	style = normal, 
	fixed = false
}).

%% The Original built-in GS Event names. 
%% THIS WILL BE REMOVED - REFERENCE PURPOSES ONLY
-define(GS_EVENTS, [
	buttonpress, buttonrelease, click, doubleclick, configure, 
	destroy, enter, focus, keypress, keyrelease, leave, motion
]).

%% GX Event Types
%% Start with the HTML 5(!) set ;-)
-define(GX_EVENTS, [ 
	onabort, onbeforeunload, onblur, onchange, onclick, oncontextmenu, 
	ondblclick, ondrag, ondragend, ondragenter, ondragleave, ondragover, 
	ondragstart, ondrop, onerror, onfocus, onhashchange, onkeydown, 
	onkeypress, onkeyup, onload, onmessage, onmousedown, onmousemove, 
	onmouseout, onmouseover, onmouseup, onmousewheel, onoffline, ononline, 
	onpopstate, onresize, onscroll, onselect, onstorage, onsubmit, onunload 
]).
