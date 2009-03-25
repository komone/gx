%%
%% GX Framework
%% Copyright 2009 <steven.charles.davis@gmail.com>. All rights reserved.
%% LICENSE: The correct license type has not yet been determined.
%%

%% GX Event Record
-record(gx, {
	id = undefined, 
	type = undefined, 
	event, % See: ?GX_EVENTS
	data = [],
	user = [],
	wx % May be REMOVED
}).

% The Original GS Event Record
%% THIS WILL BE REMOVED - REFERENCE PURPOSES ONLY
% {gs, IdOrName, EventType, Data, Args}
-record(gs, {
	id = undefined, % IdOrName
	event = undefined, % see GS_EVENTS 
	data = [], % user defined data
	args = [] % event attributes
}).

% GX Font Record
-record(font, {
	family, 
	face = undefined,
	size = undefined,
	weight = undefined, 
	style = normal, 
	fixed = false
}).

%% The Original GS font record. 
%% THIS WILL BE REMOVED - REFERENCE PURPOSES ONLY
-record(gs_font, {family, style, size}).

% Note: this definition is for documentation purposes only 
% It is not (yet/ever) used in the GX implementation
-define(GX_OBJECTS, [
	alert, box, button, canvas, checkbox, checklist, choice,
	colordialog, combo, dialog, editor, filedialog, fontdialog,
	item, label, line, list, menu, menubar, menuitem, toolbar,
	tool, panel, radiobox, radiobutton, separator, slider,
	spinner, splashscreen, statusbar, tabs, window
]).

%% The Original built-in GS Object names. 
%% THIS WILL BE REMOVED - REFERENCE PURPOSES ONLY
-define(GS_OBJECTS, [
	arc, button, canvas, editor, entry, frame, grid, gridline, 
	image, label, line, listbox, menu, menubar, menubutton, 
	menuitem, oval, polygon, rectangle, scale, text, window
]).

% Note: this definition is for documentation purposes only 
% It is not (yet/ever) used in the GX implementation
-define(GX_ATTRIBUTES, [
	align,
	border,
	choices,
	color,
	fill,
	height, 
	id,
	icon,
	image,
	items,
	label,
	layout, % column | row | horizontal | vertical | grid | flexgrid | gridbag
	max,
	min,
	pos,
	show,
	title,
	type,
	value,
	width,
	wrap
}).

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

%% The Original GS Event names. 
%% THIS WILL BE REMOVED - REFERENCE PURPOSES ONLY
%% Remove later...
-define(GS_EVENTS, [
	buttonpress, buttonrelease, click, doubleclick, configure, 
	destroy, enter, focus, keypress, keyrelease, leave, motion
]).
