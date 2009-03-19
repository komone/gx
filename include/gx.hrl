%%
%%
%%
-record(gx, {
	id = undefined, 
	type = undefined, 
	event = undefined,
	data = [],
	user = [],
	wx = undefined
}).

-record(attr, {
	id = undefined, 
	label = "",
	icon = "",
	width = -1, 
	height = -1, 
	pos = {-1,-1},
	align = center,
	fill = false,
	border = 0,
	orientation = horizontal, 
	% Consider instead 'layout' -> horizontal | vertical | grid | flexgrid | gridbag
	color = {0, 0, 0, 1},
	events = []
}).

-record(font, {
	family, 
	face = undefined, 
	size = undefined, 
	weight = undefined, 
	style = normal, 
	fixed = false
}).


%%
%% Start with the HTML 5(!) set ;-)
%%
-define(GX_EVENTS, [ 
	onabort, onbeforeunload, onblur, onchange, onclick, oncontextmenu, 
	ondblclick, ondrag, ondragend, ondragenter, ondragleave, ondragover, 
	ondragstart, ondrop, onerror, onfocus, onhashchange, onkeydown, 
	onkeypress, onkeyup, onload, onmessage, onmousedown, onmousemove, 
	onmouseout, onmouseover, onmouseup, onmousewheel, onoffline, ononline, 
	onpopstate, onresize, onscroll, onselect, onstorage, onsubmit, onunload 
]).


%% TODO: The Original GS Event names. 
%% Remove later...
-define(GS_EVENTS, [
	buttonpress, buttonrelease, click, doubleclick, configure, 
	destroy, enter, focus, keypress, keyrelease, leave, motion
]).

