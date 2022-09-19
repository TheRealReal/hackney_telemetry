%%% @doc Telemetry adapter for Hackney metrics.
%%%
%%% To use it, configure Hackney mod_metrics to use this module and
%%% make sure that the hackney_telemetry application starts before your
%%% application.
%%%
%%% Hackney calls the module specified by mod_metrics to report
%%% instrumentation metrics. This module receives the data from Hackney and
%%% passes it to a hackney_telemetry_worker which keeps the current state of
%%% the metric and generates Telemetry events.
%%%
%%% This module implements all the callbacks required by hackney_metrics.
%%%
%%% Hackney supports storing metrics in Folsom or Exometer. Unfortunately,
%%% these libraries libraries do not export data in a way that is useful for
%%% Telemetry, so we need to transform the metrics data before reporting it.
%%%
%%% Modules such as [Telemetry.Metrics](https://hex.pm/packages/telemetry_metrics)
%%% can create gauges and histograms, so we just need to keep track of
%%% metric values and report them to Telemetry.
%%%
%%% Metrics are identified by a name, which is a list of atoms or charlist
%%% strings, e.g.:
%%%
%%% - [hackney, free_count]
%%% - [hackney_pool, api_graphql, free_count]
%%%
%%% Metrics also have type: counter, histogram, gauge, or meter.
%%%
%%% For more information see:
%%% - https://github.com/benoitc/hackney/blob/master/README.md#metrics
%%% @end

-module(hackney_telemetry).

-export(
  [
    new/2,
    delete/1,
    increment_counter/1,
    increment_counter/2,
    decrement_counter/1,
    decrement_counter/2,
    update_histogram/2,
    update_meter/2,
    update_gauge/2
  ]
).

-include("hackney_telemetry.hrl").

%% @doc Create metric worker.
%%
%% Hackney calls this function when it creates a new pool. It spawns a worker
%% process to handle the new metrics.
%%
%% Hackney general metrics are ignored here because they are already included
%% in the hackney_telemetry_sup supervisor.
%%
%% @end
-spec new(metric_type(), hackney_metric()) -> ok.
new(_Type, [hackney, _key]) -> ok;

new(_Type, [hackney_pool, PoolName, _] = Metric) when is_atom(PoolName) ->
  hackney_telemetry_sup:start_worker(Metric);

new(_Type, _Metric) -> ok.

%% @doc Delete metric worker.
-spec delete(hackney_metric()) -> ok.
delete(Metric) -> hackney_telemetry_sup:stop_worker(Metric).

%% @doc Increment counter metric by 1.
-spec increment_counter(hackney_metric()) -> ok.
increment_counter(Metric) -> increment_counter(Metric, 1).

%% @doc Increment counter metric by the given value.
-spec increment_counter(hackney_metric(), non_neg_integer()) -> ok.
increment_counter(Metric, Value) ->
  hackney_telemetry_worker:update(Metric, Value, fun sum/2),
  ok.

%% @doc Decrement counter metric by 1.
-spec decrement_counter(hackney_metric()) -> ok.
decrement_counter(Metric) -> decrement_counter(Metric, 1).

%% @doc Decrement counter metric by the given value.
-spec decrement_counter(hackney_metric(), non_neg_integer()) -> ok.
decrement_counter(Metric, Value) -> hackney_telemetry_worker:update(Metric, Value * -1, fun sum/2).

%% @doc Update histogram metric.
-spec update_histogram(hackney_metric(), any()) -> ok.
update_histogram(Metric, Fun) when is_function(Fun) ->
  hackney_telemetry_worker:update(Metric, Fun, fun eval_and_replace/2);

% In Hackney, the following metrics have their value off by -1:
% - [hackney_pool, <pool_name>, free_count]
% - [hackney_pool, <pool_name>, in_use_count]
%
% For these metrics, we fix their value by adding +1.
%
% Reference: https://github.com/benoitc/hackney/blob/592a00720cd1c8eb1edb6a6c9c8b8a4709c8b155/src/hackney_pool.erl#L597-L604
update_histogram([hackney_pool, _, MetricName] = Metric, Value) ->
  FixedValue =
    case lists:member(MetricName, [in_use_count, free_count]) of
      true -> Value + 1;
      false -> Value
    end,
  hackney_telemetry_worker:update(Metric, FixedValue, fun replace/2);

update_histogram(Metric, Value) -> hackney_telemetry_worker:update(Metric, Value, fun replace/2).

%% @doc Update meter metric.
%%
%% A meter is a type of counter that only goes forward.
%% @end
-spec update_meter(hackney_metric(), any()) -> ok.
update_meter(Metric, Value) -> hackney_telemetry_worker:update(Metric, Value, fun sum/2).

%% @doc Update gauge metric.
%%
%% Gauges only keep the latest value, so we just need to replace the old state.
%%
%% @end
-spec update_gauge(hackney_metric(), any()) -> ok.
update_gauge(Metric, Value) -> hackney_telemetry_worker:update(Metric, Value, fun replace/2).

%% Transform functions

sum(StateValue, EventValue) -> StateValue + EventValue.

replace(_StateValue, EventValue) -> EventValue.

eval_and_replace(_StateValue, Fun) -> Fun().
