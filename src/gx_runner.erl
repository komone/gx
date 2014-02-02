%% Copyright 2010-2014 Steve Davis <steve@simulacity.com>
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%% 
%% http://www.apache.org/licenses/LICENSE-2.0
%% 
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(gx_runner).

-include("gx.hrl").
%-include_lib("wx/include/wx.hrl").
-include_lib("wx/src/wxe.hrl").

-export([start/0, start/1, start/2, init/2, terminate/2, browse/1]).

-export([splash/1, on_init/1, on_close/2, on_exit/2, on_edit/1,
		on_about/2, on_open/2, on_font/2, on_color/2, 
		on_browse/2, on_message/2, on_click/2, on_ackey/2, on_browse2/1]).

%%
start() ->
	gx:start(?MODULE, "www/gx/browser.xml").

%%
start(File) ->
	gx:start(?MODULE, File). 
start(Name, File) ->
	gx:start(Name, ?MODULE, File).

%%
%% Callbacks
%%
splash(_Event) ->
	gx:splashscreen(get(gx), [{image, "gx.png"}, {timeout, 3000}]).

init(_Evt, Args) ->
	State = Args,
	{ok, State}.

terminate(_, _) ->
	ok.

on_init(Event) ->
	?TTY({on_init, Event}).

on_close(Event, State) -> 
	?TTY({on_close, Event}),
	{exit, State}.

on_exit(Event, State) -> 
	?TTY({on_exit, Event}),
	{exit, State}.
	
on_edit(Event) ->
	?TTY({on_edit, Event}).

on_about(Event, State) ->
	?TTY({on_about, Event}),
	Attrs = ?MODULE:module_info(attributes),
	_Version = proplists:get_value(vsn, Attrs),
%    OS = wx_misc:getOsDescription(),
	Text = <<"GX Runner, Version 1.0\nAuthor: Steve Davis\n">>,
	gx:dialog(message, window, [{title, <<"About GX Runner">>}, {message, Text}]),
	{ok, State}.

on_open(Event, State) ->
	?TTY({Event, State}),
	case gx:dialog(file, window) of
	Path when is_binary(Path) -> 
		?TTY({file, Path}),
		gx:config(editor, load, Path);
	Other ->
		?TTY(Other)
	end,
	{ok, State}.

on_font(_Event, State) ->
	Font = #font{} = gx:dialog(font, window),
	gx:config(editor, font, Font),
	?TTY(Font),
	{ok, State}.

on_color(_Event, State) ->
	Color = gx:dialog(color, window, []),
	?TTY({color, Color}),
	{ok, State}.

on_browse(#gx{data=[Resource, _,_]}, State) ->
	browse(Resource), 
	{ok, State}.

on_ackey(#gx{data = {stc_charadded, $:, 0,E}}, State) ->
	?TTY(E),
	%Pos = gx:read(editor, pos),
	
	gx:config(editor, autocomplete, [<<"cat">>, <<"dog">>, <<"mouse">>, <<"rabbit">>]),
	{ok, State};
on_ackey(_, State) ->
	{ok, State}.
%
on_message(Event, State) ->
	G = get(gx),
	?TTY({on_message, G, Event}),
	{ok, State}.

on_click(_Event, State) -> 
	TextEntry = gx:lookup(url),
	URL = wxTextCtrl:getValue(TextEntry),
	browse(URL), 
	{ok, State}.

on_browse2(Evt) ->
	Ubf = gx_ubf:encode(Evt),
	Response = httpc:request(post, {"http://localhost:8000", 
		[{"content-type", "application/ubf"}],
		"application/ubf", Ubf}, [{timeout, 3000}], []),
	case Response of
	{ok, {{_, 200, _}, _, Ubf2}} ->	
		Rec = gx_ubf:decode(Ubf2),
		?TTY({response, Rec});
	{ok, {Status, _, _}} ->
		?TTY({response, Status});
	{error, Reason} -> 
		?TTY({error, Reason})
	end.

%%
browse(URL) ->
	case httpc:request(get, {URL, [{"connection", "close"}]}, [], []) of
	{ok, {{_, 200, _}, _, XML}} ->	
		{ok, TermList} = gx_xml:parse(XML),
		gx:start(list_to_atom(filename:basename(URL, ".xml")), ?MODULE, TermList);
	{ok, {Status, _, _}} ->
		?TTY({response, Status});
	{error, Reason} -> 
		?TTY({error, Reason})
	end.
	
