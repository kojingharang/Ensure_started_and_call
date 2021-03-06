%% @doc Application module for `server_a'

-module(ensure_call_app).

-behaviour(application).

%%----------------------------------------------------------------------------------------------------------------------
%% 'application' Callback API
%%----------------------------------------------------------------------------------------------------------------------
-export([start/2, stop/1]).

%%----------------------------------------------------------------------------------------------------------------------
%% 'application' Callback Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @private
start(_StartType, _StartArgs) ->
    server_a_sup:start_link().

%% @private
stop(_State) ->
    ok.
