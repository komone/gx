%%
%%
%%
-record(gx, {id, type, event, data}).

%% TODO: The Original GS Event names. 
%% Remove later...
-define(GS_EVENTS, [
	buttonpress, buttonrelease, click, doubleclick, configure, 
	destroy, enter, focus, keypress, keyrelease, leave, motion
]).

%%
%% Start with the HTML 5(!) set
%%
-define(GX_EVENTS, [ 
	onabort, onbeforeunload, onblur, onchange, onclick, oncontextmenu, 
	ondblclick, ondrag, ondragend, ondragenter, ondragleave, ondragover, 
	ondragstart, ondrop, onerror, onfocus, onhashchange, onkeydown, 
	onkeypress, onkeyup, onload, onmessage, onmousedown, onmousemove, 
	onmouseout, onmouseover, onmouseup, onmousewheel, onoffline, ononline, 
	onpopstate, onresize, onscroll, onselect, onstorage, onsubmit, onunload 
]).
