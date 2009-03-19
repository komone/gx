%%
%%
-module(gx_runner).
-vsn("0.1").
-author("steve@simulacity.com").

-export([start/1]).
-compile(export_all).

%%
start(File) ->
	gx:start(?MODULE, File). 

%%
%% Callbacks
%%
splash(Gx, _Event) ->
	gx:splashscreen(Gx, [{image, "gx.png"}, {timeout, 3000}]).
	
on_init(_Gx, Event) ->
	print(Event).

on_close(_Gx, Event) -> 
	print(Event), exit.

on_exit(_Gx, Event) -> 
	print(Event).

on_about(Gx, _Event) ->
	Attrs = ?MODULE:module_info(attributes),
	Version = proplists:get_value(vsn, Attrs),
	Author = proplists:get_value(author, Attrs),
    OS = wx_misc:getOsDescription(),
	Text = lists:flatten(["GX Runner, Version ", Version, 
		"\nAuthor: ", Author, "\n", OS]),
	gx:alert(Gx, Text, [{title, "About GX Runner"}]).

on_open(Gx, _Event) ->
	String = gx:filedialog(Gx, []),
	io:format("FILE CHOSEN ~p~n", [String]).

on_font(Gx, _Event) ->
	Font = gx:fontdialog(Gx, []),
	io:format("FONT CHOSEN ~p~n", [Font]).

on_color(Gx, _Event) ->
	Color = gx:colordialog(Gx, []),
	io:format("COLOR CHOSEN ~p~n", [Color]).

on_message(_Gx, Event) ->
	print(Event).
	%gx:set(win, status, ["Last event at " ++ timestamp()]).

on_click(_Gx, _Event) -> 
	io:format("[GXRUNNER] Clicked!~n", []).

print(Message) ->
	io:format("[GXRUNNER] ~p~n", [Message]).

%%
%% Application Internals
%%

%% creates a valid, printable RFC 3339 (ISO 8601) timestamp
timestamp() ->
	{{Y, M, D}, {H, M1, S}} = calendar:universal_time(),
	L = io_lib:format(
		"~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0B.0Z", 
		[Y, M, D, H, M1, S]),
	lists:flatten(L).
