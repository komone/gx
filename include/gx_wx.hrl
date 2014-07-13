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

-include_lib("wx/include/wx.hrl").
-include_lib("wx/src/wxe.hrl").

% GX to WX Mapping Record
-record(gx_wx, {type, attributes, module = ?MODULE, extends, wx_type, wx_parent, event_map}).

% GX Component Record
-record(gx_ui, {id, ref, parent, module = ?MODULE}).

% GX Cache Record
-record(gx_cache, {constants, types, modules, resources}).

-define(DEFAULT_RESOURCE_PATH, <<"./priv/icons">>).

-ifndef(wxEventListener_new_0).
-define(wxEventListener_new_0, 98).
-endif.

-ifndef(wxEventListener_Destroy_1).
-define(wxEventListener_Destroy_1, 99).
-endif.
