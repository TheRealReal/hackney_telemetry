![Tests](https://github.com/TheRealReal/hackney_telemetry/actions/workflows/ci.yml/badge.svg)
[![Contributor Covenant](https://img.shields.io/badge/Contributor%20Covenant-2.1-4baaaa.svg)](CODE_OF_CONDUCT.md)

# hackney_telemetry

Telemetry adapter for Hackney metrics.

This module is a [metrics handler](https://github.com/benoitc/hackney/blob/master/README.md#metrics)
for the [Hackney](https://github.com/benoitc/hackney) HTTP client. It receives
calls from Hackney to update metrics and generates [Telemetry](https://github.com/beam-telemetry/telemetry) events.

Hackney supports storing metrics in [Folsom](https://hex.pm/packages/folsom) or
[Exometer](https://hex.pm/packages/exometer_core). Unfortunately, these
libraries libraries do not export data in a way that is useful for Telemetry,
so we need to transform the metrics data before reporting it.

## Telemetry metrics

The following metrics are exported by this library to telemetry.

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

This module implements all the callbacks required by `hackney_metrics` but it does
not support host metrics.

To use it, configure Hackney `mod_metrics` to use this module and make sure
that the `hackney_telemetry` application starts before your application.

Hackney calls the module specified by `mod_metrics` to report instrumentation
metrics. This module receives the data from Hackney and passes it to a
`hackney_telemetry_worker` which keeps the current state of the metric and
generates Telemetry events.

A worker process has two jobs:

1.  Calculate metric values

    Hackney does not keep the state of its metrics, but instead emits events to
    the metrics engine, like "increase this counter by 1", "set this gauge to X",
    "add Y to this histogram". The job of a metric worker is to process these
    events and keep up-to-date state representing the value of the tracked metric.
    State updates run in constant time (O(1)), important since a single
    request generates about nine metric updates.

2.  Send metric values to Telemetry

    If we send the metric value to Telemetry after every update, then
    telemetry processing may not be able to keep up, and Telemetry will apply
    backpressure.

    Since the worker maintains the most up-to-date value, we can send the current
    value periodically. Gauge metrics may be less accurate, but it avoids overload.

## Installation

Install it from [Hex](https://hex.pm/packages/hackney_telemetry) or
[Github](https://github.com/TheRealReal/hackney_telemetry).

## Configuration

Configure set Hackney to send metrics to this module:

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

By default, workers will report data to telemetry every 1000 milliseconds.
If set to 0, events are generated after every update.

You can change that by setting the `report_interval` option:

**Erlang**

```erlang
{hackney_telemetry, [{report_interval, 2000}]}
```

**Elixir**

```elixir
config :hackney_telemetry, report_interval: 2_000
```

## Usage

After installing the module, your application will receive Telemetry events.
You can handle them in your application, or install a reporting module such
as [Telemetry.Metrics](https://hex.pm/packages/telemetry_metrics)
or [prom_ex](https://hex.pm/packages/prom_ex).

## Elixir

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

## Building

To build the source code locally you'll need [rebar3](https://github.com/erlang/rebar3):

```
rebar3 compile
```

## Test

```console
rebar3 ct
```

## Docs

```console
rebar3 ex_doc
```

## Format code

```console
rebar3 steamroll
```

## Code of Conduct

This project  Contributor Covenant version 2.1. Check [CODE_OF_CONDUCT.md](CODE_OF_CONDUCT.md) file for more information.

## License

`hackney_telemetry` source code is released under Apache License 2.0.

Check [NOTICE](NOTICE) and [LICENSE](LICENSE) files for more information.
