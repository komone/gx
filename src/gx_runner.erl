%%
%% GX Framework
%% Copyright (c) 2009 Steve Davis <steven.charles.davis@gmail.com>. All rights reserved.
%% LICENSE: Creative Commons Non-Commercial License V 3.0 
%% http://creativecommons.org/licenses/by-nc/3.0/us/
%%
-module(gx_runner).
-vsn("0.3").
-author('steve@simulacity.com').
-include("../include/gx.hrl").
-export([start/0, start/1, browse/1]).
-compile(export_all).

%%
%%

start() ->
	gx:start(?MODULE, "www/gx/browser.xml").

%%
start(File) ->
	gx:start(?MODULE, File). 
start(Name, File) ->
	gx:start(Name, ?MODULE, File).


%%
browse(URL) ->
	case http:request(get, {URL, [{"connection", "close"}]}, [], []) of
	{ok, {{_, 200, _}, _, XML}} ->	
		{ok, TermList} = gx_xml:parse(XML),
		gx:start(list_to_atom(filename:basename(URL, ".xml")), ?MODULE, TermList);
	{ok, {Status, _, _}} ->
		io:format("Response: ~p~n", [Status]);
	{error, Reason} -> 
		io:format("Request Error: ~p~n", [Reason])
	end.

%%
%% Callbacks
%%
splash(Gx, _Event) ->
	gx:splashscreen(Gx, [{image, "gx.png"}, {timeout, 3000}]).
	
on_init(_Gx, Event) ->
	print(Event).

on_close(_Gx, Event) -> 
	print(Event), 
	exit.

on_exit(_Gx, Event) -> 
	print(Event),
	exit.

on_about(Gx, Event) ->
	print(Event),
	Attrs = ?MODULE:module_info(attributes),
	Version = proplists:get_value(vsn, Attrs),
    OS = wx_misc:getOsDescription(),
	Text = lists:flatten(["GX Runner, Version ", Version, 
		"\nAuthor: Steve Davis\n", OS]),
	gx:alert(Gx, Text, [{title, "About GX Runner"}]).

on_open(Gx, _Event) ->
	String = gx:filedialog(Gx, []),
	io:format("FILE CHOSEN ~p~n", [String]).

on_font(Gx, _Event) ->
	Font = #font{} = gx:fontdialog(Gx, []),
	io:format("FONT CHOSEN ~p~n", [Font]).

on_color(Gx, _Event) ->
	Color = gx:colordialog(Gx, []),
	io:format("COLOR CHOSEN ~p~n", [Color]).

on_browse(_Gx, #gx{data=[Resource, _,_]}) ->
	browse(Resource), 
	ok.

%
on_message(_Gx, Evt = #gx{}) ->
	Ubf = ubf:encode(Evt),
	Response = http:request(post, {"http://localhost:8000", 
		[{"content-type", "application/ubf"}],
		"application/ubf", Ubf}, [{timeout, 3000}], []),
	case Response of
	{ok, {{_, 200, _}, _, Ubf2}} ->	
		{done, Rec, []} = ubf:decode(Ubf2),
		io:format("~p: ~p~n", [?MODULE, Rec]);
	{ok, {Status, _, _}} ->
		io:format("Response: ~p~n", [Status]);
	{error, Reason} -> 
		io:format("Request Error: ~p~n", [Reason])
	end,
	ok.

on_click(_Gx, _Event) -> 
	TextEntry = gx:lookup(url),
	URL = wxTextCtrl:getValue(TextEntry),
	browse(URL), 
	ok.

print(Message) ->
	io:format("~p ~p~n", [self(), Message]).

