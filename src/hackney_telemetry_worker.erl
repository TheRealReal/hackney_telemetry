%%%=============================================================================
%%% @doc Worker process to handle hackney metrics
%%%
%%% A worker process has two jobs:
%%%
%%%  (1) Calculate the metric value.
%%%
%%%  Hackney does not keep the state of its metrics, but instead emits events to
%%%  the metrics engine, like "+1 on this counter", "set X on this gauge", "add
%%%  Y on this histogram". The job of a metric worker is to process these events
%%%  and keep an up-to-date state that represent the real value of the tracked
%%%  metric. State updates runs on constant time - aka O(1) complexity, which is
%%%  important since each single request generates about 9 metric updates.
%%%
%%%  (2) Send the metric value to Telemetry
%%%
%%%  Telemetry will apply backpressure if we send too many events - which may
%%%  cause the process inbox to grow if we try to send the metric value after
%%%  every update. But because the state already has the most up-to-date value,
%%%  we can send this value on intervals - this does cause the metric to be less
%%%  accurate but allows us to use telemetry.
%%%
%%%  Start options:
%%%
%%% - metric: the name of the metric, as a list of atoms. It's required.
%%% - report_interval: the interval, in milliseconds, that a worker reports data
%%%   to telemetry. If set to 0, scheduled reports are disabled and the worker
%%%   will report after every update. Defaults to the value configured in the
%%%   hackney_telemetry/report_interval config.
%%%
%%% @end
%%%=============================================================================

-module(hackney_telemetry_worker).

-behaviour(gen_server).

% gen_server callbacks
-export(
  [code_change/3, init/1, handle_call/3, handle_cast/2, handle_info/2, start_link/1, terminate/2]
).

% public functions
-export([child_spec/1, update/3, worker_name/1]).

-record(
  state,
  {
    value :: any(),
    report_interval :: non_neg_integer(),
    telemetry_settings :: {[atom(), ...], atom(), map()}
  }
).

-include("hackney_telemetry.hrl").

%%------------------------------------------------------------------------------
%% @doc Generates a worker child spec based on a
%% @end
%%------------------------------------------------------------------------------

-spec child_spec(hackney_metric()) -> map().
child_spec(Args) ->
  Metric = proplists:get_value(metric, Args),
  #{id => {?MODULE, Metric}, start => {?MODULE, start_link, [Args]}}.

%%------------------------------------------------------------------------------
%% @doc Returns the name of the worker process
%% @end
%%------------------------------------------------------------------------------

-spec worker_name(hackney_metric()) -> {global, {atom(), hackney_metric()}}.
worker_name(Metric) -> {global, {node(), Metric}}.

%%-----------------------------------------------------------------------------
%% @doc Updates a metric
%% @end
%%-----------------------------------------------------------------------------

-spec update(hackney_metric(), any(), transform_fun()) -> ok.
update(Metric, EventValue, TransformFun) ->
  ProcessName = worker_name(Metric),
  gen_server:cast(ProcessName, {update_event, EventValue, TransformFun}).

%%-----------------------------------------------------------------------------
%% @doc Starts the server
%% @end
%%-----------------------------------------------------------------------------

start_link(Args) ->
  Metric = proplists:get_value(metric, Args),
  gen_server:start_link(worker_name(Metric), ?MODULE, Args, []).

%%-----------------------------------------------------------------------------
%% @doc Initialize the state of the server
%% @end
%%-----------------------------------------------------------------------------

init(Args) ->
  case telemetry_settings(Args) of
    {ok, TelemetrySettings} ->
      State =
        #state{
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

%%-----------------------------------------------------------------------------
%% @doc gen_server handle_call implementation.
%% @end
%%-----------------------------------------------------------------------------

handle_call(_Message, _From, State) -> {reply, ok, State}.

%%-----------------------------------------------------------------------------
%% @doc Handles update events
%% @end
%%-----------------------------------------------------------------------------

handle_cast({update_event, EventValue, TransformFun}, State) ->
  NewValue = TransformFun(State#state.value, EventValue),
  UpdatedState = State#state{value = NewValue},
  if UpdatedState#state.report_interval > 0 -> report(UpdatedState) end,
  {noreply, UpdatedState}.

%%-----------------------------------------------------------------------------
%% @doc Handles report events
%% @end
%%-----------------------------------------------------------------------------

handle_info(report, State) ->
  report(State),
  maybe_schedule_report(State),
  {noreply, State}.

%%------------------------------------------------------------------------------
%% @doc gen_server terminate calback
%% @end
%%------------------------------------------------------------------------------

terminate(_Reason, _State) -> ok.

%%------------------------------------------------------------------------------
%% @doc gen_server code_change calback
%% @end
%%------------------------------------------------------------------------------

code_change(_OldVersion, State, _Extra) -> {ok, State}.

%%------------------------------------------------------------------------------
%% @doc Evaluates the current state of the process and report the value to
%% telemetry
%% @end
%%------------------------------------------------------------------------------

-spec report(#state{}) -> ok.
report(State) ->
  {Metric, MeasurementKey, Metadata} = State#state.telemetry_settings,
  Measurement = #{MeasurementKey => State#state.value},
  telemetry:execute(Metric, Measurement, Metadata),
  ok.

%%------------------------------------------------------------------------------
%% @doc Reports events to telemetry If the report interval is greater than zero,
%% we schedule the report to happen.
%% @end
%%------------------------------------------------------------------------------

-spec maybe_schedule_report(#state{}) -> ok.
maybe_schedule_report(State) ->
  case State of
    #{report_interval := ReportInterval} when ReportInterval > 0 ->
      erlang:send_after(ReportInterval, self(), report),
      ok;

    _any -> ok
  end.

%%------------------------------------------------------------------------------
%% @doc Fetches the interval on which workers report metrics to telemetry.
%% @end
%%------------------------------------------------------------------------------

-spec fetch_report_interval(#state{}) -> ok.
fetch_report_interval(Args) ->
  ValueFromArgs = proplists:get_value(report_interval, Args),
  ValueFromConfig = application:get_env(hackney_telemetry, report_interval),
  if
    ValueFromArgs =/= undefined -> ValueFromArgs;
    true -> ValueFromConfig
  end.
