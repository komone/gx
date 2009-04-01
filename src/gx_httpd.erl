%%
%% GX Framework
%% Copyright (c) 2009 Steve Davis <steven.charles.davis@gmail.com>. All rights reserved.
%% LICENSE: Creative Commons Non-Commercial License V 3.0 
%% http://creativecommons.org/licenses/by-nc/3.0/us/
%%
-module(gx_httpd).

-compile(export_all).
-export([start/0, start/1, start_ssl/0, stop/0]).
-export([do/1]).
-define(DEFAULT_PORT, 8000).

-include("../include/gx.hrl").

-include_lib("inets/src/httpd.hrl").

%
start() ->
	start(?DEFAULT_PORT).
%	
start(Port) when is_integer(Port) ->
	inets:start(),
	{ok, Pid} = inets:start(httpd, [
		{port, Port},
		{server_name,"localhost"}, 
		{server_root,"priv"},
		{document_root,"priv/www"}, 
		{bind_address, any},
		{modules, [
			mod_alias,
			mod_dir, 
			mod_get, 
			mod_head, 
			gx_httpd,
			mod_log
		]},
		{directory_index, ["index.html"]},
%		{ssl_certificate_file, "../ssl/cert.pem"},
		{error_log, "logs/error.log"},
		{error_log, "logs/security.log"},
		{transfer_log, "logs/access.log"},
		{mimetypes, [ 
			{"xml", "application/xml"},
			{"ubf", "application/ubf"},
			{"html", "text/html"},
			{"css", "text/css"}, 
			{"js", "text/javascript"} 
		]}
	]),
	%register(gx_httpd, Pid),
	Pid.

% Seems to just 'lock up'
start_ssl() ->
	inets:start(),
	{ok, Pid} = inets:start(httpd, [
		{port, 8443},
		{server_name,"localhost"}, 
		{server_root,"priv"},
		{document_root,"priv/www"}, 
		{bind_address, any},
		{modules, [mod_head, mod_get, mod_log, gx_httpd]},
		{socket_type, ssl},
		{ssl_certificate_file, "../ssl/cert.pem"},
		{ssl_ca_certificate_file, "../ssl/cacerts.pem"},
%		{ssl_certificate_key_file, "../ssl/key.pem"}
		{error_log, "priv/logs/error.log"},
		{error_log, "priv/logs/security.log"},
		{transfer_log, "priv/logs/access.log"},
		{mimetypes, [
			{"xml", "text/xml"},
			{"ubf", "application/ubf"},
			{"html", "text/html"},
			{"css", "text/css"}, 
			{"js", "text/javascript"}
		]}
	]),
	register(gx_httpd, Pid),
	Pid.
	
stop() ->
	case whereis(gx_httpd) of 
	undefined -> ok;
	_ -> inets:stop(gx_httpd)
	end,
	inets:stop().

%
do(Request) ->
    case Request#mod.method of
	"POST" ->
		case proplists:get_value(status, Request#mod.data) of
		{_StatusCode, _, _} -> 
			{proceed, Request#mod.data};
		%% No status code has been generated!
		_ ->
			case proplists:get_value(response, Request#mod.data) of
			%% No response has been generated!
			undefined ->
				Body = process(Request#mod.entity_body), % At last
				Response = {response, [
					{code, 200}, 
					{content_type, "application/ubf"}, 
					{content_length, integer_to_list(length(Body))}
					], Body},
				{proceed,[{response, Response}]};
			%% A response has been sent! Nothing to do about it!
			{already_sent, _StatusCode, _Size} ->
				{proceed, Request#mod.data};
			%% A response has been generated!
			{_StatusCode, _Response} ->
				{proceed, Request#mod.data}
			end
		end;
	_ ->
	    {proceed, Request#mod.data}
    end.

%placeholder
process(UBF) ->
	case ubf:decode(UBF) of
	{done, #gx{}, []} -> ubf:encode({gx, {status, ok}});
	_ -> ubf:encode({gx, {status, nak}})
	end.
