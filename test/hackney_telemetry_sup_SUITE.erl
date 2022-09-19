-module(hackney_telemetry_sup_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

%% Setup/Teardown functions

all() -> [starts_and_stops_workers].

init_per_suite(Config) ->
  application:ensure_all_started(telemetry),
  Config.


end_per_suite(Config) ->
  application:stop(telemetry),
  Config.


init_per_testcase(_, Config) ->
  {ok, Pid} = hackney_telemetry_sup:start_link(),
  [{sup_pid, Pid} | Config].


end_per_testcase(_, Config) ->
  SupPid = ?config(sup_pid, Config),
  exit(SupPid, shutdown),
  Config.

%% Tests

starts_and_stops_workers(_Config) ->
  Metric = [hackney, spool],
  [_, _, _] = supervisor:which_children(hackney_telemetry_sup),
  ok = hackney_telemetry_sup:start_worker(Metric),
  [{{hackney_telemetry_worker, [hackney, spool]}, WorkerPid, _, _}, _, _, _] =
    supervisor:which_children(hackney_telemetry_sup),
  ok = hackney_telemetry_sup:stop_worker(Metric),
  timer:sleep(15),
  false = is_process_alive(WorkerPid),
  [_, _, _] = supervisor:which_children(hackney_telemetry_sup).
