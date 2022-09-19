-record(worker_state,
        {value :: any(),
         report_interval :: non_neg_integer(),
         telemetry_settings :: {[atom(), ...], atom(), map()}}).
