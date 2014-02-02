%% Copyright 2010-2011 Steve Davis <steve@simulacity.com>
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

-module(gx_ui_controlwithitems).

-include("gx.hrl").
-include("gx_wx.hrl").

-export([read/3, config/4]).

read(Gx, Ref, Key) ->
	gx_ui_control:read(Gx, Ref, Key).

config(Gx, Ref, K, V) ->
	gx_ui_control:config(Gx, Ref, K, V).
