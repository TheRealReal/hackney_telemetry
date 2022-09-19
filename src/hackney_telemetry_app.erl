%%%-----------------------------------------------------------------------------
%%% @doc hackney_telemetry application
%%% @end
%%%-----------------------------------------------------------------------------

-module(hackney_telemetry_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    hackney_telemetry_sup:start_link().

stop(_State) ->
    ok.
