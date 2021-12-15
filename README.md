![Tests](https://github.com/TheRealReal/hackney_telemetry/actions/workflows/ci.yml/badge.svg)
[![Contributor Covenant](https://img.shields.io/badge/Contributor%20Covenant-2.1-4baaaa.svg)](CODE_OF_CONDUCT.md)

# hackney_telemetry

`hackney_telemetry` will automatically compute and translate [hackney](https://github.com/benoitc/hackney) metrics to something that [telemetry](https://github.com/beam-telemetry/telemetry) understands.

## Telemetry metrics

The following metrics are exported by this library to telemetry. They will represent the last known value of the metric. They are based on [hackney's metrics](https://github.com/benoitc/hackney#metrics).

| Metric                      | Tags | Meaning                                               |
| --------------------------- | ---- | ----------------------------------------------------- |
| `hackney.nb_requests`       | -    | Current number of requests                            |
| `hackney.finished_requests` | -    | Total number of finished requests                     |
| `hackney.total_requests`    | -    | Total number of requests                              |
| `hackney_pool.free_count`   | pool | Number of free sockets in a connection pool           |
| `hackney_pool.in_use_count` | pool | Number of busy sockets in a connection pool           |
| `hackney_pool.no_socket`    | pool | Count of new connections                              |
| `hackney_pool.queue_count`  | pool | Number of requests waiting for a connection in a pool |
| `hackney_pool.take_rate`    | pool | Rate at which a connection is retrieved from the pool |

*Note: metrics for hosts are still not supported*.

## Installation

Install it from [Hex](https://hex.pm/packages/hackney_telemetry) or [Github](https://github.com/TheRealReal/hackney_telemetry).

## Usage

### Configuring Telemetry

You'll likely use `telemetry` together with a reporting library.

**Elixir**

Use it with [Telemetry.Metrics](https://hex.pm/packages/telemetry_metrics) library.

```elixir
defmodule YourApplcation.Telemetry do
  import Telemetry.Metrics

  def metrics do
  [
    # other metrics

    last_value("hackney.nb_requests"),
    last_value("hackney.finished_requests"),
    last_value("hackney.total_requests"),
    last_value("hackney_pool.free_count", tags: [:pool]),
    last_value("hackney_pool.in_use_count", tags: [:pool]),
    last_value("hackney_pool.no_socket", tags: [:pool]),
    last_value("hackney_pool.queue_count", tags: [:pool]),
    last_value("hackney_pool.take_rate", tags: [:pool])
  ]
  end
end
```

### Configuring Hackney

In your application config you need to set hackney's `mod_metrics`:

**Erlang**

```erlang
{hackney, [{mod_metrics, hackney_telemetry}]}
```

**Elixir**

```elixir
config :hackney, mod_metrics: :hackney_telemetry
```

### Options

#### Report interval

By default, workers will report data to telemetry every 1000 milliseconds. You
can change that by setting the `report_interval` option:

**Erlang**

```erlang
{hackney_telemetry, [{report_interval, 2_000}]}
```

**Elixir**

```elixir
config :hackney_telemetry, report_interval: 2_000
```

## Building

To build the source code locally you'll need [rebar3](https://github.com/erlang/rebar3):

```
rebar3 compile
```

## Code of Conduct

This project  Contributor Covenant version 2.1. Check [CODE_OF_CONDUCT.md](/CODE_OF_CONDUCT.md) file for more information.

## License

`hackney_telemetry` source code is released under Apache License 2.0.

Check [NOTICE](/NOTICE) and [LICENSE](/LICENSE) files for more information.
