%%%-----------------------------------------------------------------------------
%%% @doc hackney_telemetry application supervisor.
%%% @end
%%%-----------------------------------------------------------------------------

-module(hackney_telemetry_sup).

-behaviour(supervisor).

% supervisor callbacks
-export([init/1]).

% public api
-export([start_link/0, start_worker/1, stop_worker/1]).

-define(SERVER, ?MODULE).

-include("hackney_telemetry.hrl").

start_link() -> supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
  SupFlags = #{strategy => one_for_one, intensity => 0, period => 1},
  ChildSpecs =
    [
      hackney_telemetry_worker:child_spec([{metric, [hackney, nb_requests]}]),
      hackney_telemetry_worker:child_spec([{metric, [hackney, total_requests]}]),
      hackney_telemetry_worker:child_spec([{metric, [hackney, finished_requests]}])
    ],
  {ok, {SupFlags, ChildSpecs}}.

%%%=============================================================================
%%% @doc Starts a worker for the given metric under this supervisor
%%% @end
%%%=============================================================================

-spec start_worker(hackney_metric()) -> ok.
start_worker(Metric) ->
  ChildSpec = hackney_telemetry_worker:child_spec([{metric, Metric}]),
  supervisor:start_child(?MODULE, ChildSpec),
  ok.

%%%=============================================================================
%%% @doc Stops the worker associated with the given metric name, if it exists.
%%% @end
%%%=============================================================================

-spec stop_worker(hackney_metric()) -> ok.
stop_worker(Metric) ->
  WorkerName = hackney_telemetry_worker:worker_name(Metric),
  case global:whereis_name(WorkerName) of
    undefined -> ok;

    WorkerPid ->
      supervisor:terminate_child(WorkerPid),
      ok
  end.
