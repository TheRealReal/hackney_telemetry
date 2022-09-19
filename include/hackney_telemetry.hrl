-type hackney_global_measurements() :: finished_requests | nb_requests | total_requests.
-type hackney_host_measurements() :: connect_error
                                   | connect_time
                                   | connect_timeout
                                   | nb_requests
                                   | request_time
                                   | response_time.
-type hackney_pool_measurements() :: free_count
                                   | in_use_count
                                   | no_socket
                                   | queue_count
                                   | take_rate.
-type hackney_global_metric() :: list().
-type hackney_host_metric() :: list().
-type hackney_pool_metric() :: list().

-type hackney_generic_metrics() :: nb_requests | total_requests | finished_requests.
-type hackney_host_metrics() :: nb_requests | request_time | connect_time | response_time | connect_timeout | connect_error | new_connection | reuse_connection.
-type hackney_pool_metrics() :: take_rate | no_socket | in_use_count | free_count | queue_count.

% -type hackney_metric() :: [hackney, hackney_generic_metrics()] | [hackney, any(), hackney_host_metrics()] | [hackney_pool, atom(), hackney_pool_metrics()].
-type hackney_metric() :: list().

-type metric_type() :: counter | gauge | histogram | meter.
-type transform_fun() :: fun((any(), any()) -> any()).

-type keyword() :: {atom(), any()}.
-type keywords() :: list(keyword()).
