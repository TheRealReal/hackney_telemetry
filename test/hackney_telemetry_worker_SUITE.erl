-module(hackney_telemetry_worker_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

all() -> [scheduled_reports, state_updates].

%% Setup/Teardown functions

init_per_suite(Config) ->
  application:ensure_all_started(telemetry),
  Config.


end_per_suite(Config) ->
  application:stop(telemetry),
  Config.


init_per_testcase(_, Config) ->
  ok = telemetry:attach("test_handler", [hackney], fun send_to_self/4, self()),
  Config.


end_per_testcase(_, Config) ->
  telemetry:detach("test_handler"),
  Config.

%% Tests

scheduled_reports(_) ->
  Metric = [hackney, test1],
  WorkerOptions = [{metric, Metric}, {report_interval, 50}],
  {ok, _Pid} = hackney_telemetry_worker:start_link(WorkerOptions),
  hackney_telemetry_worker:update(Metric, 42, fun replace/2),
  receive
    {telemetry_event, [hackney], #{test1 := 42}, #{}} -> ok;
    _ -> ct:fail(unexpected_message)
  after
    100 -> ct:fail(timeout)
  end.


state_updates(_) ->
  Metric = [hackney, test2],
  WorkerOptions = [{metric, Metric}, {report_interval, 0}],
  {ok, _Pid} = hackney_telemetry_worker:start_link(WorkerOptions),
  hackney_telemetry_worker:update(Metric, 99, fun replace/2),
  receive
    {telemetry_event, [hackney], #{test2 := 99}, #{}} -> ok;
    _ -> ct:fail(unexpected_message)
  after
    50 -> ct:fail(timeout)
  end.

%% Helpers

% This function is attached to telemetry and will send a message to the process
% running the test so we can assert over update events.
send_to_self(Metric, Measurement, Metadata, TestPid) ->
  TestPid ! {telemetry_event, Metric, Measurement, Metadata},
  ok.


replace(_Old, New) -> New.
