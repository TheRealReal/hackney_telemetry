%%%=============================================================================
%%% @doc Adapter module for hackney metrics
%%%
%%% To use it, configure hackney;s `mod_metrics` to use `hackney_telemetry` and
%%% make sure that hackney_telemetry starts before your application.
%%%
%%% This module implements all callbacks required by the hackney's
%%% `hackney_metrics` module and is called by hackney to report its
%%% instrumentation metrics.
%%%
%%% Each metric is identified by a list of atoms/charlist. Some examples:
%%% - [hackney, free_count]
%%% - [hackney_pool, api_graphql, free_count]
%%%
%%% Each metric has a type - counter, histogram, gauge or meter - and hackney
%%% leverages the actual interpretation of data to external libraries - like
%%% folsom or exometer. Unfortunatelly, these libs do not export data in a way
%%% that is useful for telemetry so we need to transform this data before
%%% reporting them. Fortunatelly, telemetry are able to create gauges and
%%% histograms, so we just need to keep track of metric values and report to
%%% telemetry.
%%%
%%% This module will receive the data from hackney and delegate the reports to
%%% `hackney_telemetry_worker`, providing all the required transformations so
%%% the data is correctly exported to telemetry.
%%%
%%% For more information please refer to the following document:
%%% - https://github.com/benoitc/hackney/blob/master/README.md%metrics
%%%
%%% @end
%%%=============================================================================

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

%%------------------------------------------------------------------------------
%% @doc Handles metric worker creation.
%%
%% Called when hackney creates new pools to spawn a worker process to
%% handle the new metrics.
%%
%% Hackney general metrics are ignored here because they are already included
%% in hackney_telemetry_sup supervisor.
%%
%% @end
%%------------------------------------------------------------------------------

-spec new(metric_type(), hackney_metric()) -> ok.
new(_Type, [hackney, _key]) -> ok;

new(_Type, [hackney_pool, PoolName, _] = Metric) when is_atom(PoolName) ->
  hackney_telemetry_sup:start_worker(Metric);

new(_Type, _Metric) -> ok.

%%------------------------------------------------------------------------------
%% @doc Handles metric worker deletion.
%% @end
%%------------------------------------------------------------------------------

-spec delete(hackney_metric()) -> ok.
delete(Metric) -> hackney_telemetry_sup:stop_worker(Metric).

%%------------------------------------------------------------------------------
%% @doc Increments a counter metric by 1.
%% @end
%%------------------------------------------------------------------------------

-spec increment_counter(hackney_metric()) -> ok.
increment_counter(Metric) -> increment_counter(Metric, 1).

%%------------------------------------------------------------------------------
%% @doc Increments a counter metric by the given value.
%% @end
%%------------------------------------------------------------------------------

-spec increment_counter(hackney_metric(), non_neg_integer()) -> ok.
increment_counter(Metric, Value) ->
  hackney_telemetry_worker:update(Metric, Value, fun sum/2),
  ok.

%%------------------------------------------------------------------------------
%% @doc Decrements a counter metric by 1.
%% @end
%%------------------------------------------------------------------------------

-spec decrement_counter(hackney_metric()) -> ok.
decrement_counter(Metric) -> decrement_counter(Metric, 1).

%%------------------------------------------------------------------------------
%% @doc Decrements a counter metric by the given value.
%% @end
%%------------------------------------------------------------------------------

-spec decrement_counter(hackney_metric(), non_neg_integer()) -> ok.
decrement_counter(Metric, Value) -> hackney_telemetry_worker:update(Metric, Value * -1, fun sum/2).

%%------------------------------------------------------------------------------
%% @doc Updates a histogram metric
%% @end
%%------------------------------------------------------------------------------

-spec update_histogram(hackney_metric(), any()) -> ok.
update_histogram(Metric, Fun) when is_function(Fun) ->
  hackney_telemetry_worker:update(Metric, Fun, fun eval_and_replace/2);

% Hackney will make the following metrics have a shift of -1 on their value:
% - [hackney_pool, <pool_name>, free_count]
% - [hackney_pool, <pool_name>, in_use_count]
%
% For these metrics, we fix their value by adding +1.
%
% Reference: https://github.com/benoitc/hackney/blob/592a00720cd1c8eb1edb6a6c9c8b8a4709c8b155/src/hackney_pool.erl%L597-L604
update_histogram([hackney_pool, _, MetricName] = Metric, Value) ->
  FixedValue =
    case lists:member(MetricName, [in_use_count, free_count]) of
      true -> Value + 1;
      false -> Value
    end,
  hackney_telemetry_worker:update(Metric, FixedValue, fun replace/2);

update_histogram(Metric, Value) -> hackney_telemetry_worker:update(Metric, Value, fun replace/2).

%%------------------------------------------------------------------------------
%% @doc Updates a meter metric
%%
%% A meter is a type of counter that only goes forward.
%%
%% @end
%%------------------------------------------------------------------------------

-spec update_meter(hackney_metric(), any()) -> ok.
update_meter(Metric, Value) -> hackney_telemetry_worker:update(Metric, Value, fun sum/2).

%%------------------------------------------------------------------------------
%% @doc Updates a gauge metric.
%%
%% Gauges only keep the latest value, so we just need to replace the old state.
%%
%% @end
%%------------------------------------------------------------------------------

-spec update_gauge(hackney_metric(), any()) -> ok.
update_gauge(Metric, Value) -> hackney_telemetry_worker:update(Metric, Value, fun replace/2).

%% Transform functions

sum(StateValue, EventValue) -> StateValue + EventValue.

replace(_StateValue, EventValue) -> EventValue.

eval_and_replace(_StateValue, Fun) -> Fun().
