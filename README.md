# opentelemetry_nova

OpenTelemetry instrumentation for the [Nova](https://github.com/novaframework/nova) web framework.

Automatic HTTP request tracing and metrics with Prometheus export support.

## Installation

Add to your dependencies in `rebar.config`:

```erlang
{deps, [
    {opentelemetry_nova, "~> 0.1"}
]}.
```

## Setup

Call `setup/0` during application startup:

```erlang
opentelemetry_nova:setup().
```

Or with Prometheus export:

```erlang
opentelemetry_nova:setup(#{prometheus => #{port => 9464}}).
```

## Metrics

The following metrics are collected automatically:

| Metric | Type | Description |
|--------|------|-------------|
| `http.server.request.duration` | Histogram (seconds) | Request duration |
| `http.server.active_requests` | UpDown Counter | Currently active requests |
| `http.server.request.body.size` | Histogram (bytes) | Request body size |
| `http.server.response.body.size` | Histogram (bytes) | Response body size |

## Components

- **`otel_nova_stream_h`** - Cowboy stream handler for HTTP tracing and metrics
- **`otel_nova_plugin`** - Nova plugin for span enrichment with controller/action attributes
- **`otel_nova_prom_exporter`** - Prometheus exporter with delta-to-cumulative conversion
- **`otel_nova_prom_server`** - HTTP server for Prometheus scraping

## License

Apache 2.0
