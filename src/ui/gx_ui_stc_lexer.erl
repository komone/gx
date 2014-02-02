%% Copyright 2011 Steve Davis <steve@simulacity.com>
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


-module(gx_ui_stc_lexer).

-include("gx.hrl").
-include("gx_wx.hrl").

-export([init/3]).

%TEMP
-compile(export_all).

-define(KEYWORDS, <<"after and andalso band begin bnot bor bsl bsr bxor case catch cond "
	"div end fun if let not of or orelse query receive rem try when xor">>).	
-define(ATTRIBUTES, <<"-behavior -behaviour -compile -created -created_by -export -file -import "
	"-include -include_lib -module -modified -modified_by -record -revision -spec -type -vsn">>).
-define(PREPROCESSOR, <<"-define -else -endif -ifdef -ifndef -include -include_lib -undef">>).
-define(BUILTIN, <<"erlang: abs adler32 adler32_combine erlang:append_element apply "
	"atom_to_binary atom_to_list binary_to_atom binary_to_existing_atom binary_to_list "
	"bitstring_to_list binary_to_term bit_size erlang:bump_reductions byte_size "
	"erlang:cancel_timer check_process_code concat_binary crc32 crc32_combine date "
	"decode_packet delete_module erlang:demonitor disconnect_node erlang:display element "
	"erase erlang:error exit float float_to_list erlang:fun_info erlang:fun_to_list "
	"erlang:function_exported garbage_collect get erlang:get_cookie get_keys "
	"erlang:get_stacktrace group_leader halt erlang:hash hd erlang:hibernate "
	"integer_to_list erlang:integer_to_list iolist_to_binary iolist_size is_alive "
	"is_atom is_binary is_bitstring is_boolean erlang:is_builtin is_float is_function "
	"is_integer is_list is_number is_pid is_port is_process_alive is_record is_reference "
	"is_tuple length link list_to_atom list_to_binary list_to_bitstring list_to_existing_atom "
	"list_to_float list_to_integer erlang:list_to_integer list_to_pid list_to_tuple load_module "
	"erlang:load_nif erlang:loaded erlang:localtime erlang:localtime_to_universaltime make_ref "
	"erlang:make_tuple erlang:max erlang:md5 erlang:md5_final erlang:md5_init erlang:md5_update "
	"erlang:memory erlang:min module_loaded erlang:monitor monitor_node node nodes now open_port "
	"erlang:phash erlang:phash2 pid_to_list port_close port_command erlang:port_command "
	"port_connect port_control erlang:port_call erlang:port_info erlang:port_to_list "
	"erlang:ports pre_loaded erlang:process_display process_flag process_info processes "
	"purge_module put erlang:raise erlang:read_timer erlang:ref_to_list register registered "
	"erlang:resume_process round self erlang:send erlang:send_after erlang:send_nosuspend "
	"erlang:set_cookie setelement size spawn spawn_link spawn_monitor spawn_opt split_binary "
	"erlang:start_timer statistics erlang:suspend_process erlang:system_flag erlang:system_info "
	"erlang:system_monitor erlang:system_profile term_to_binary throw time tl erlang:trace "
	"erlang:trace_delivered erlang:trace_info erlang:trace_pattern trunc tuple_size tuple_to_list "
	"erlang:universaltime erlang:universaltime_to_localtime unlink unregister whereis erlang:yield">>).

% Default style style.erlang.0=fore:#000000
% Default comment style.erlang.1=$(font.comment),fore:#7f7f7f,italics
% Variable style.erlang.2=fore:#008080
% Number style.erlang.3=fore:#007f00
% Reserved words style.erlang.4=fore:#5f0033,bold
% String style.erlang.5=fore:#0033cc
% Operators style.erlang.6=fore:#000000
% Atom style.erlang.7=fore:#000000
% Function name style.erlang.8=fore:#00007f
% Character style.erlang.9=fore:#007f00
% Macro style.erlang.10=fore:#7f007f
% Record style.erlang.11=fore:#007f00
% Preprocessor style.erlang.12=fore:#5f0033,bold
% Node name style.erlang.13=fore:#007f7f
% Function comment style.erlang.14=$(font.comment),fore:#7f7f7f,italics
% Module comment style.erlang.15=$(font.comment),fore:#7f7f7f,italics
% Documentation helper in comment style.erlang.16=$(font.comment),fore:#6f8fdf,italics
% Documentation macro in comment style.erlang.17=fore:#3f5fbf,bold,italics
% 18
% Macro quoted style.erlang.19=fore:#7f007f
% Record quoted style.erlang.20=fore:#007f00
% Atom quoted style.erlang.18=fore:#7f7f00
% Node name quoted style.erlang.21=fore:#007f7f
% 22
% Module name style.erlang.23=fore:#000000
% Built-in functions style.erlang.22=fore:#00007f
% Module attributes style.erlang.24=fore:#5f0033,bold

%-define(wxSTC_ERLANG_DEFAULT, 0).
%-define(wxSTC_ERLANG_COMMENT, 1).
%-define(wxSTC_ERLANG_VARIABLE, 2).
%-define(wxSTC_ERLANG_NUMBER, 3).
%-define(wxSTC_ERLANG_KEYWORD, 4).
%-define(wxSTC_ERLANG_STRING, 5).
%-define(wxSTC_ERLANG_OPERATOR, 6).
%-define(wxSTC_ERLANG_ATOM, 7).
%-define(wxSTC_ERLANG_FUNCTION_NAME, 8).
%-define(wxSTC_ERLANG_CHARACTER, 9).
%-define(wxSTC_ERLANG_MACRO, 10).
%-define(wxSTC_ERLANG_RECORD, 11).
%-define(wxSTC_ERLANG_SEPARATOR, 12).
%-define(wxSTC_ERLANG_NODE_NAME, 13).
%-define(wxSTC_ERLANG_UNKNOWN, 31).

% LexErlang.cxx
%typedef enum {
%   STATE_NULL,
%   ATOM_UNQUOTED,
%   ATOM_QUOTED,
%   ATOM_FUN_NAME,
%   NODE_NAME_UNQUOTED,
%   NODE_NAME_QUOTED,
%   MACRO_START,
%   MACRO_UNQUOTED,
%   MACRO_QUOTED,
%   RECORD_START,
%   RECORD_UNQUOTED,
%   RECORD_QUOTED,
%   NUMERAL_START,
%   NUMERAL_SIGNED,
%   NUMERAL_RADIX_LITERAL,
%   NUMERAL_SPECULATIVE_MANTISSA,
%   NUMERAL_FLOAT_MANTISSA,
%   NUMERAL_FLOAT_EXPONENT,
%   NUMERAL_FLOAT_SIGNED_EXPONENT,
%   PARSE_ERROR
%} atom_parse_state_t;

init(Gx, Ref = #wx_ref{type = wxStyledTextCtrl}, erlang) ->
%	?TTY({prelexer, read(Gx, Ref, lexer)}),
	gx_wx:cast(Gx, ?wxStyledTextCtrl_SetLexer, [Ref, ?wxSTC_LEX_ERLANG]),
	gx_wx:cast(Gx, ?wxStyledTextCtrl_SetWordChars, [Ref, <<"-_abcdefghijklmnopqrstuvxwyzABCDEFGHIJKLMNOPQRSTUVWXYZ">>]),
	gx_wx:cast(Gx, ?wxStyledTextCtrl_SetKeyWords, [Ref, 0, ?KEYWORDS]),
	gx_wx:cast(Gx, ?wxStyledTextCtrl_SetKeyWords, [Ref, 1, ?BUILTIN]),
	gx_wx:cast(Gx, ?wxStyledTextCtrl_SetKeyWords, [Ref, 2, ?PREPROCESSOR]),
	gx_wx:cast(Gx, ?wxStyledTextCtrl_SetKeyWords, [Ref, 3, ?ATTRIBUTES]),
%	?TTY({postlexer, read(Gx, Ref, lexer)}),
	set_style(Gx, Ref, ?wxSTC_ERLANG_COMMENT, <<"face:Source Code Pro,size:11,fore:#7f7f7f">>),	
	set_style(Gx, Ref, ?wxSTC_ERLANG_VARIABLE, <<"fore:#000080">>),
	set_style(Gx, Ref, ?wxSTC_ERLANG_NUMBER, <<"fore:#007f00">>),	
	set_style(Gx, Ref, ?wxSTC_ERLANG_KEYWORD, <<"face:Source Code Pro,size:11,bold,fore:#5f0033">>),	
	set_style(Gx, Ref, ?wxSTC_ERLANG_STRING, <<"fore:#0f0fef">>),
	set_style(Gx, Ref, ?wxSTC_ERLANG_OPERATOR, <<"fore:#000000">>),
	set_style(Gx, Ref, ?wxSTC_ERLANG_ATOM, <<"fore:#000000">>),
	set_style(Gx, Ref, ?wxSTC_ERLANG_FUNCTION_NAME, <<"fore:#00007f">>),
	set_style(Gx, Ref, ?wxSTC_ERLANG_CHARACTER, <<"fore:#007f00">>),
	set_style(Gx, Ref, ?wxSTC_ERLANG_MACRO, <<"fore:#7f007f">>),
	set_style(Gx, Ref, ?wxSTC_ERLANG_RECORD, <<"fore:#007f00">>),
	set_style(Gx, Ref, ?wxSTC_ERLANG_SEPARATOR, <<"face:Source Code Pro,size:11,bold,fore:#5f0033">>),
	set_style(Gx, Ref, ?wxSTC_ERLANG_NODE_NAME, <<"fore:#007f7f">>),
	set_style(Gx, Ref, 14, <<"face:Source Code Pro,size:11,italic,fore:#7f7f7f">>),
	set_style(Gx, Ref, 15, <<"face:Source Code Pro,size:14,italic,fore:#7f7f7f">>),
	set_style(Gx, Ref, 16, <<"face:Source Code Pro,size:14,italic,fore:#6f8fdf">>),
	set_style(Gx, Ref, 17, <<"face:Source Code Pro,size:14,bold,italic,fore:#3f5fbf">>),
	set_style(Gx, Ref, 18, <<"fore:#7f7f00">>),
	set_style(Gx, Ref, 19, <<"fore:#7f007f">>),
	set_style(Gx, Ref, 20, <<"fore:#007f00">>),
	set_style(Gx, Ref, 21, <<"fore:#007f7f">>),
	set_style(Gx, Ref, 22, <<"fore:#00007f">>),
	set_style(Gx, Ref, 23, <<"fore:#007f00">>),
	set_style(Gx, Ref, 24, <<"face:Source Code Pro,size:11,bold,fore:#5f0033">>), % attributes
	{ok, erlang};
init(_, _, _) ->
	{ok, none}.
%
set_style(Gx, Ref, Style, Spec) ->
	gx_wx:cast(Gx, ?wxStyledTextCtrl_StyleSetSpec, [Ref, Style, Spec]).

set_bold(Gx, Ref, Style, Boolean) ->
	gx_wx:cast(Gx, ?wxStyledTextCtrl_StyleSetBold, [Ref, Style, Boolean]).
set_italic(Gx, Ref, Style, Boolean) ->
	gx_wx:cast(Gx, ?wxStyledTextCtrl_StyleSetItalic, [Ref, Style, Boolean]).

set_background(Gx, Ref, Style, Color) ->
	gx_wx:cast(Gx, ?wxStyledTextCtrl_StyleSetBackground, [Ref, Style, Color]).
set_foreground(Gx, Ref, Style, Color) ->
	gx_wx:cast(Gx, ?wxStyledTextCtrl_StyleSetForeground, [Ref, Style, Color]).
