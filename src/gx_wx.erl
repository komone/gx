%% Copyright 2010-2014 Steve Davis <steve@simulacity.com>
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

-module(gx_wx).

-include("gx.hrl").
-include("gx_wx.hrl").

-export([base_command_id/0, null/0, batch/2, call/3, cast/3, bind_event/4, destroy/2]).

-export([encode_args/1]).

%%
base_command_id() ->
	?wxID_HIGHEST + 1001.

%%	
null() ->
	#wx_ref{ref = 0, type = wx}.

%%
batch(#g{port = Port}, Fun) when is_function(Fun) ->
    ok = wxe_cast(Port, ?BATCH_BEGIN, <<>>),
	try 
		Fun()
    catch 
		error:W -> 
			erlang:exit({W, erlang:get_stacktrace()});
		throw:W -> 
			erlang:throw(W);
		exit:W -> 
			erlang:exit(W)
    after 
        ok = wxe_cast(Port, ?BATCH_END, <<>>)
    end.

%%
call(#g{port = Port}, Operation, Args) when is_binary(Args) ->
	wxe_call(Port, Operation, Args);
call(#g{port = Port}, Operation, Args) when is_list(Args) ->
	%ok = ?VALIDATE(call, Operation, Args, encode_args(Args)),
	wxe_call(Port, Operation, encode_args(Args)).

%%		
cast(#g{port = Port}, Operation, Args) ->
	%ok = ?VALIDATE(cast, Operation, Args, encode_args(Args)),
	wxe_cast(Port, Operation, encode_args(Args)).

%%
bind_event(#g{port = Port, event_listener = #wx_ref{ref = EL, type = wxeEvtListener}}, #wx_ref{ref = Ref, type = Source}, Event, Data) ->
	WinId = -1, % any
	LastId = -1, % any
	SkipFlag = 0, % false
	UserDataFlag = 1, % true
	FunFlag = 0, % false
	Bin = list_to_binary([atom_to_list(Event)|[0]]),
	Bin0 = list_to_binary([atom_to_list(Source)|[0]]),	
	Args = <<
		EL:32/unsigned-native, 
		Ref:32/unsigned-native, 
		WinId:32/unsigned-native, 
		LastId:32/unsigned-native,
		SkipFlag:32/unsigned-native, 
		UserDataFlag:32/unsigned-native, 
		FunFlag:32/unsigned-native,
		(size(Bin)):32/unsigned-native, 
		(size(Bin0)):32/unsigned-native, 
		Bin/binary, 
		Bin0/binary
	>>,
	true = erlang:port_command(Port, term_to_binary(Data)),
	wxe_call(Port, ?wxEvtHandler_Connect, Args).

%%
destroy(#g{port = Port}, #wx_ref{ref = Ref}) ->
	wxe_cast(Port, ?DESTROY_OBJECT, <<Ref:32/unsigned-native>>).

% 
% internal functions
% 
encode_args([#wx_ref{ref = Ref, type = _Type}, Bin]) when byte_size(Bin) =:= 16 ->
%	?TTY(Type, Bin),
	list_to_binary([<<Ref:32/unsigned-native>>, Bin]);
%% Second arg is string...
encode_args([#wx_ref{ref = Ref}, Bin | Args]) when is_binary(Bin) ->
	Acc = pad64(list_to_binary([<<Ref:32/unsigned-native>>, encode(Bin)])),
	encode_args(Args, Acc);
encode_args([#wx_ref{ref = Ref}, #wx_ref{ref = Ref0}, Bin | Args]) when is_binary(Bin) ->
	Acc = pad64(list_to_binary([<<Ref:32/unsigned-native>>, <<Ref0:32/unsigned-native>>, encode(Bin)])),
	encode_args(Args, Acc);
encode_args([#wx_ref{ref = Ref}, X, Bin | Args]) when is_integer(X), is_binary(Bin) ->
	Acc = pad64(list_to_binary([<<Ref:32/unsigned-native>>, <<X:32/unsigned-native>>, encode(Bin)])),
	encode_args(Args, Acc);
encode_args([#wx_ref{ref = Ref}, #wx_ref{ref = Ref2} | Args]) ->
	encode_args(Args, <<Ref:32/unsigned-native, Ref2:32/unsigned-native>>);
encode_args([#wx_ref{ref = Ref}, X | Args]) when is_integer(X) ->
	encode_args(Args, <<Ref:32/unsigned-native, X:32/unsigned-native>>);
encode_args([X, #wx_ref{ref = Ref} | Args]) when is_integer(X) ->
	encode_args(Args, <<X:32/unsigned-native, Ref:32/unsigned-native>>);
encode_args([#wx_ref{ref = Ref}, Opts = {options, _, _} | Args]) ->
	encode_args([Opts|Args], <<Ref:32/unsigned-native, 0:32>>);
encode_args([#wx_ref{ref = Ref} | Args]) ->
	Bin = encode_args(Args, <<>>), 
	<<Ref:32/unsigned-native, Bin/binary>>;
encode_args(Args) when is_list(Args) ->
	encode_args(Args, <<>>).
	
encode_args([H|T], Acc) ->
	Acc0 = pad64(Acc),
	Arg = encode(H),
	encode_args(T, <<Acc0/binary, Arg/binary>>);
encode_args([], Acc) ->
	Acc.

%%
encode(#wx_ref{ref = Ref}) ->
	<<Ref:32/unsigned-native>>;
encode({options, Keys, Values}) ->
	encode_options(Keys, Values);
encode(X) when is_integer(X) ->
%	?TTY({int32, Value}),
	<<X:32/unsigned-native>>;
encode({long, X}) ->
	<<X:64/unsigned-native>>;
encode(Value) when is_float(Value) ->
	<<Value:64/float-native>>;	
encode(true) ->
	<<1:32/unsigned-native>>;
encode(false) ->
	<<0:32>>;
% NOTE: default this temporarily
encode(center) ->
	<<-1:32/unsigned-native, -1:32/unsigned-native>>;
encode({X, Y}) when is_integer(X) andalso is_integer(Y) ->
	<<X:32/unsigned-native, Y:32/unsigned-native>>;	
%encode({event, EventType, Source}) ->
%	Bin = list_to_binary([atom_to_list(EventType)|[0]]),
%	Bin0 = list_to_binary([atom_to_list(Source)|[0]]),
%	<<(size(Bin)):32/unsigned-native, (size(Bin0)):32/unsigned-native, Bin/binary, Bin0/binary>>;
encode({R, G, B}) ->
	encode({R, G, B, 255});
encode({R, G, B, A}) ->
	<<R:32/unsigned-native, G:32/unsigned-native, B:32/unsigned-native, A:32/unsigned-native>>;	
encode({{Y, Mo, D}, {H, M, S}}) ->
	%% NOTE: wxDaytime constructor uses D, M, Y rather than Y, M, D. 
	%% NOTE: wxDaytime:Month is enumerated from zero
	<<D:32/unsigned-native, (Mo - 1):32/unsigned-native, Y:32/unsigned-native,
		H:32/unsigned-native, M:32/unsigned-native, S:32/unsigned-native>>;
% Choice and Checklist items
encode(Value) when is_list(Value) ->
	encode_choices(Value);
% Labels
encode(Value) when is_binary(Value) ->
	Size = byte_size(Value),
	<<(Size + 1):32/unsigned-native, Value/binary, 0>>.

encode_choices(List) ->
%	?TTY({encode_choices, List}),
	Length = length(List),
	encode_choices(List, <<Length:32/unsigned-native>>).
encode_choices([H|T], Acc) ->
	Size = byte_size(H) + 1,
	encode_choices(T, <<Acc/binary, Size:32/unsigned-native, H/binary, 0>>);
encode_choices([], Acc) ->
	pad32(Acc).
	
%% NOTE: Option names are expected to presented in the argument order required by the function
encode_options(Opts, Values) ->
	% NOTE: revert to normal option ordering as this reverse is only needed 
	% to align and validate against the wxe implementation of reversed option ordering.
	encode_options(lists:reverse(Opts), Values, length(Opts), <<>>).
encode_options([H|T], Values, Count, Acc) ->
	case proplists:get_value(H, Values) of
	undefined ->
		encode_options(T, Values, Count - 1, Acc); % ignore
	Value ->
		Acc0 = pad64(Acc),
		Bin = encode(Value),
		Bin0 = <<Count:32/unsigned-native, Bin/binary>>,
		encode_options(T, Values, Count - 1, <<Acc0/binary, Bin0/binary>>)
	end;
encode_options([], _, _, Acc) ->
	Acc0 = pad64(Acc),
	<<Acc0/binary, 0:32>>.

pad32(Bin) when is_binary(Bin) ->
	Size = byte_size(Bin),
	Pad = (4 - Size band 3) band 3,
	<<Bin:Size/binary, 0:Pad/unit:8>>.
pad64(Bin) when is_binary(Bin) ->
	Size = byte_size(Bin),
	Pad = (8 - Size band 7) band 7,
	<<Bin:Size/binary, 0:Pad/unit:8>>.

%
wxe_call(Port, OpCode, Args) ->
	<<>> = erlang:port_control(Port, OpCode, Args),
    receive 
	{'_wxe_result_', Result} -> 
		Result;
	{'_wxe_error_', Code, Error} -> 
	    erlang:error({Code, Error})
    end.
%
wxe_cast(Port, OpCode, Args) ->
	<<>> = erlang:port_control(Port, OpCode, Args),
	ok.



