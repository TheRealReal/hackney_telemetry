-module(hackney_telemetry_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

%% Setup/Teardown functions

all() -> [creates_workers, telemetry_integration].

init_per_suite(Config) ->
  application:ensure_all_started(hackney_telemetry),
  Config.


end_per_suite(Config) ->
  application:stop(hackney_telemetry),
  Config.


init_per_testcase(_, Config) ->
  ok = telemetry:attach("test_handler", [hackney_pool], fun send_to_self/4, self()),
  Config.


end_per_testcase(_, Config) ->
  telemetry:detach("test_handler"),
  Config.

%% Tests

creates_workers(_) ->
  Metric = [hackney_pool, fool_pool],
  undefined = global:whereis_name({node(), Metric}),
  hackney_telemetry:new(gauge, Metric),
  undefined =/= global:whereis_name({node(), Metric}).


telemetry_integration(_) ->
  Metric = [hackney_pool, dull_pool, in_use_count],
  hackney_telemetry_worker:start_link([{metric, Metric}, {report_interval, 0}]),
  hackney_telemetry:update_gauge(Metric, 10),
  receive
    {telemetry_event, [hackney_pool], #{in_use_count := 10}, #{pool := dull_pool}} -> ok
  after
    10 -> ct:fail(message_not_received)
  end.

%% Helpers

send_to_self(Metric, Measurement, Metadata, TestPid) ->
  TestPid ! {telemetry_event, Metric, Measurement, Metadata},
  ok.
