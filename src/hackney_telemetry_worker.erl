%%% @doc Worker process to handle a hackney metric.
%%%
%%% A worker process has two jobs:
%%%
%%%  (1) Calculate metric values
%%%
%%%  Hackney does not keep the state of its metrics, but instead emits events to
%%%  the metrics engine, like "increase this counter by 1", "set this gauge to X",
%%%  "add Y to this histogram". The job of a metric worker is to process these
%%%  events and keep up-to-date state representing the value of the tracked metric.
%%%  State updates run in constant time (O(1)), important since a single
%%%  request generates about nine metric updates.
%%%
%%%  (2) Send metric values to Telemetry
%%%
%%%  If we send the metric value to Telemetry after every update, then
%%%  telemetry processing may not be able to keep up, and Telemetry will apply
%%%  backpressure.
%%%
%%%  Since the worker maintains the most up-to-date value, we can send the current
%%%  value periodically. Gauge metrics may be less accurate, but it avoids overload.
%%%
%%%  Start options:
%%%
%%% - metric: name of the metric as a list of atoms (required).
%%%
%%% - report_interval: how often the worker reports data to telemetry, in milliseconds.
%%%
%%%   If set to 0, scheduled reports are disabled and the worker
%%%   will report after every update. Defaults to the value configured in the
%%%   hackney_telemetry/report_interval config.
%%% @end

-module(hackney_telemetry_worker).

-behaviour(gen_server).

% gen_server callbacks
-export(
  [code_change/3, init/1, handle_call/3, handle_cast/2, handle_info/2, start_link/1, terminate/2]
).

% public functions
-export([child_spec/1, update/3, worker_name/1]).

-include("hackney_telemetry.hrl").
-include("hackney_telemetry_worker.hrl").

-define(DEFAULT_REPORT_INTERVAL, 5000).

%% @doc Generate worker child spec for a supervisor.

-spec child_spec(hackney_metric()) -> map().
child_spec(Args) ->
  Metric = proplists:get_value(metric, Args),
  #{id => {?MODULE, Metric}, start => {?MODULE, start_link, [Args]}}.

%% @doc Return name of the worker process

-spec worker_name(hackney_metric()) -> {global, {atom(), hackney_metric()}}.
worker_name(Metric) -> {global, {node(), Metric}}.

%% @doc Update metric

-spec update(hackney_metric(), any(), transform_fun()) -> ok.
update(Metric, EventValue, TransformFun) ->
  ProcessName = worker_name(Metric),
  gen_server:cast(ProcessName, {update_event, EventValue, TransformFun}).

%% @doc Start server

start_link(Args) ->
  Metric = proplists:get_value(metric, Args),
  gen_server:start_link(worker_name(Metric), ?MODULE, Args, []).

%% @doc Initialize server state

init(Args) ->
  case telemetry_settings(Args) of
    {ok, TelemetrySettings} ->
      State =
        #worker_state{
          value = 0,
          report_interval = fetch_report_interval(Args),
          telemetry_settings = TelemetrySettings
        },
      maybe_schedule_report(State),
      {ok, State};

    {error, Error} -> {stop, Error}
  end.


telemetry_settings(Args) ->
  Metric = proplists:get_value(metric, Args),
  case Metric of
    [hackney, MeasurementKey] -> {ok, {[hackney], MeasurementKey, #{}}};

    [hackney_pool, PoolName, MeasurementKey] ->
      {ok, {[hackney_pool], MeasurementKey, #{pool => PoolName}}};

    _ -> {error, unsupported_metric}
  end.

%% @doc gen_server handle_call implementation.

handle_call(_Message, _From, State) -> {reply, ok, State}.

%% @doc Handle update event

handle_cast({update_event, EventValue, TransformFun}, State) ->
  NewValue = TransformFun(State#worker_state.value, EventValue),
  UpdatedState = State#worker_state{value = NewValue},
  if
    UpdatedState#worker_state.report_interval == 0 -> report(UpdatedState);
    true -> ok
  end,
  {noreply, UpdatedState}.

%% @doc Handle report events

handle_info(report, State) ->
  report(State),
  maybe_schedule_report(State),
  {noreply, State}.

%% @doc gen_server terminate callback

terminate(_Reason, _State) -> ok.

%% @doc gen_server code_change callback

code_change(_OldVersion, State, _Extra) -> {ok, State}.

%% @doc Evaluate current process state and report value to telemetry.

-spec report(#worker_state{}) -> ok.
report(State) ->
  {Metric, MeasurementKey, Metadata} = State#worker_state.telemetry_settings,
  Measurement = #{MeasurementKey => State#worker_state.value},
  telemetry:execute(Metric, Measurement, Metadata),
  ok.

%% @doc Report events to telemetry.
%%
%% If the report interval is greater than zero, schedule the report to happen.
%% @end

-spec maybe_schedule_report(#worker_state{}) -> ok.
maybe_schedule_report(State) ->
  ReportInterval = State#worker_state.report_interval,
  if
    ReportInterval > 0 ->
      erlang:send_after(ReportInterval, self(), report),
      ok;

    true -> ok
  end.

%% @doc Fetch interval on which workers report metrics to telemetry.

-spec fetch_report_interval(#worker_state{}) -> ok.
fetch_report_interval(Args) ->
  ValueFromArgs = proplists:get_value(report_interval, Args),
  ValueFromConfig = application:get_env(hackney_telemetry, report_interval),
  if
    ValueFromArgs =/= undefined -> ValueFromArgs;

    ValueFromConfig =/= undefined ->
      {ok, ActualValueFromConfig} = ValueFromConfig,
      ActualValueFromConfig;

    true -> ?DEFAULT_REPORT_INTERVAL
  end.
