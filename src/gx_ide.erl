%% Copyright 2011-2014 Steve Davis <steve@simulacity.com>
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

-module(gx_ide).

-include("gx.hrl").

-export([init/2, terminate/2]).

-export([on_load/2, on_close/2, on_exit/2, on_edit/2, on_save/2,
		on_about/2, on_open/2, on_new/2, on_cancel/2, on_tree/2, on_path/2, on_font/2, on_color/2, 
		on_message/2, on_click/2, on_ackey/2, splash/2]).

-export([to_hex/1]).

init(Event, Args) ->
	State = Args,
	log({init, Event, State}),
	gx:batch(fun() ->
		% wxTree is broken as of R16B03-1 
		%gx:config(filetree, fontsize, 11),
		%gx:config(filetree, bg_color, {220, 220, 220, 255}),
		gx:config(main_split, sash, -1),
		gx:config(tree_split, sash, 300),
		gx:config(libraries, center, true)
	end),
	%gx:config(libraries, show, modal),
	{ok, State}.

terminate(Event, State) ->
	?TTY({Event, State}),
	ok.
	
on_load(Event, State) ->
	log({on_load, Event}),
	{ok, State}.

on_close(Event, State) -> 
	log({on_close, Event}),
	{exit, State}.

on_exit(Event, State) -> 
	log({on_exit, Event}),
	?TTY(exiting),
	{exit, State}.
	
on_edit(Event, State) ->
	log({on_edit, Event}),
	{ok, State}.

on_about(Event, State) ->
	log({on_about, Event}),
	Attrs = ?MODULE:module_info(attributes),
	_Version = proplists:get_value(vsn, Attrs),
%    OS = wx_misc:getOsDescription(),
	Text = <<"GX IDE, Version 1.0\nAuthor: Steve Davis\n">>,
	gx:dialog(window, message, [{title, <<"About GX IDE">>}, {message, Text}]),
	{ok, State}.

on_path(Event, State) ->
	log({Event, State}),
	case gx:dialog(window, directory) of
	Path when is_binary(Path) -> 
		Filetree = #filetree{fill = both, label = <<>>, path = Path, 
			callbacks = [{onclick,on_tree}, {ondblclick,on_tree}, {onchange,on_tree}]},
		Filename = path:name(Path),
		TabName = <<Filename/binary, ".app">>,
		Tab = #tab{label = TabName, fill = both, content = [Filetree]},
		gx:batch(fun() ->
			SashPos = gx:read(tree_split, sash),
			gx:create(app_tabs, Tab),
			gx:config(tree_split, fit),
			gx:config(window, layout),
			gx:config(tree_split, sash, SashPos)
		end),
		log({directory, Path});
	Other ->
		log(Other)
	end,
	{ok, State}.

on_new(Event, State) ->
	log({Event, State}),
	gx:config(new_module, center, true),
	gx:config(new_module, show, modal),
	{ok, State}.

on_open(Event, State) ->
	log({Event, State}),
	case gx:dialog(window, file) of
	Path when is_binary(Path) -> 
		log({file, Path}),
		Bin = path:load(Path),
		case text:is_string(Bin) of
		true ->
			gx:config(editor, text, Bin);
		false ->
			gx:config(editor, text, <<"[binary]">>)
		end;
	Other ->
		log(Other)
	end,
	{ok, State}.

on_save(Event = #gx{ref = Ref, data = _Data}, State) ->
	Bin = gx:read(editor, text),
	{current, File} = lists:keyfind(current, 1, State),
	case lists:member(path:extension(File), [<<"erl">>, <<"app">>, <<"txt">>, <<"xml">>]) of 
	true ->
		ok = path:save(File, Bin),
		Compile = path:extension(File) =:= <<"erl">>,
		log({Event, File, Compile, byte_size(Bin)}),
		case Compile of 
		true ->
			Result = compile:file(binary_to_list(File), [{i, "include"}, {outdir, "ebin"}, return]),
			case Result of
			{ok, Module, []} ->
				_Success = code:purge(Module),
				{module, Module} = code:load_file(Module),
				log(Result),
				gx:config(main_split, sash, -1);
			{ok, Module, Warnings} ->
				_Success = code:purge(Module),
				{module, Module} = code:load_file(Module),
				[log(Warning) || Warning <- Warnings],
				[?TTY({warning, Warning}) || Warning <- Warnings],
				gx:config(main_split, sash, 0);
			{error, Errors, Warnings} ->
				gx:config(main_split, sash, 0),
				[log(Error) || Error <- Errors],
				[?TTY({error, Error}) || Error <- Errors],
				[log(Warning) || Warning <- Warnings],
				[?TTY({warning, Warning}) || Warning <- Warnings]
			end;
		false ->
			log({File, not_compiled}),
			ok
		end,
		{ok, State};
	false ->
		log({unsupported_file_type, File}),
		gx:dialog(message, Ref, [{title, <<"File type not supported">>}, {message, File}]),
		{ok, State}
	end.

on_tree(Event = #gx{data = Data}, State) ->
	log({on_tree, Event, State}),
	case proplists:get_value(value, Data, undefined) of
	{regular, File} ->
		Bin = path:load(File),
		case text:is_string(Bin) of
		true ->
			Editable = true,
			Bin0 = Bin;
		false ->
			Editable = false,
			Bin0 = to_hex(Bin)
		end,
		Label = path:filename(File),
		gx:config(editor, editable, true),
		gx:config(editor, text, Bin0),
		gx:config(editor, editable, Editable),
		gx:config(editor_tabs, label, Label),
		State0 = lists:keystore(current, 1, State, {current, File});
	_ ->
		State0 = State
	end,

	{ok, State0}.

on_cancel(Event, State) ->
	log({Event, State}),
	gx:config(new_module, show, false),
	gx:config(libraries, show, false),
	{ok, State}.

on_font(_Event, State) ->
	Font = #font{} = gx:dialog(window, font),
	gx:config(editor, font, Font),
	log(Font),
	{ok, State}.

on_color(_Event, State) ->
	Color = gx:dialog(window, color, []),
	log({color, Color}),
	{ok, State}.

on_ackey(#gx{data = {stc_charadded, $:, 0, E}}, State) ->
	log(E),
	%Pos = gx:read(editor, pos),
	gx:config(editor, autocomplete, [<<"cat">>, <<"dog">>, <<"mouse">>, <<"rabbit">>]),
	{ok, State};
on_ackey(_, State) ->
	{ok, State}.
%
on_message(Event, State) ->
	log({on_message, Event}),
	{ok, State}.

on_click(Event, State) -> 
	log({on_click, Event}),
	{ok, State}.

%
splash(_Event, State) ->
	gx:splashscreen([{image, "gx.png"}, {timeout, 3000}]),
	{ok, State}.

log(Message) ->
	Bin = text:format(Message),
	gx:config(log, append, <<"$ ", Bin/binary, $\n>>).

	
to_hex(Bin) ->
	Hex = hex:encode(Bin),
	Lines = to_lines(Hex, []),
	list_to_binary(to_chunks(Lines, [])).

to_lines(<<X:32/binary, Bin/binary>>, Acc) ->
	to_lines(Bin, [X|Acc]);
to_lines(Bin, Acc) ->
	lists:reverse([Bin|Acc]).

to_chunks([H|T], Acc) ->
	C = to_chunk(H, <<>>),
	to_chunks(T, [C|Acc]);
to_chunks([], Acc) ->
	lists:reverse(Acc).

to_chunk(<<>>, Acc) ->
	<<Acc/binary, "\n">>;
to_chunk(<<X:2/binary>>, Acc) ->
	<<Acc/binary, X/binary, "\n">>;
to_chunk(<<X:2/binary, Bin/binary>>, Acc) ->
	to_chunk(Bin, <<Acc/binary, X/binary, " ">>).
