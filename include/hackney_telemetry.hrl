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
-type hackney_metric() :: hackney_global_metric | hackney_host_metric | hackney_pool_metric.
-type metric_type() :: counter | gauge | histogram | meter.
-type transform_fun() :: fun((any(), any()) -> any()).
