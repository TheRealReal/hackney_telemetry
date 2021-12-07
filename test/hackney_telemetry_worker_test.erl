-module(hackney_telemetry_worker_test).

-include_lib("eunit/include/eunit.hrl").

start_link_test() ->
  Result = hackney_telemetry_worker:start_link([{metric, [hackney, nb_requests]}]),
  ?assertMatch({ok, _Pid}, Result).
