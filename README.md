# opentelemetry_nova

OpenTelemetry instrumentation for the [Nova](https://github.com/novaframework/nova) web framework.

Automatic HTTP request tracing and metrics following the [OTel HTTP semantic conventions](https://opentelemetry.io/docs/specs/semconv/http/).

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

## Span Attributes

Server spans are created for every HTTP request with the following attributes:

| Attribute | Source |
|-----------|--------|
| `http.request.method` | Request method |
| `http.response.status_code` | Response status code |
| `http.route` | Matched route template (e.g. `/users/:id`) |
| `url.path` | Request path |
| `url.scheme` | `http` or `https` |
| `server.address` | Host header |
| `server.port` | Server port |
| `client.address` | `X-Forwarded-For` / `Forwarded` header, or peer IP |
| `network.peer.address` | Peer IP address |
| `network.peer.port` | Peer port |
| `network.protocol.version` | HTTP version (`1.0`, `1.1`, `2`, `3`) |
| `user_agent.original` | User-Agent header |
| `error.type` | Status code as string on 5xx errors |
| `nova.app` | Nova application name |
| `nova.controller` | Controller module |
| `nova.action` | Action function |

Span names follow the `{METHOD} {route}` convention (e.g. `GET /users/:id`).

## Metrics

| Metric | Type | Attributes |
|--------|------|------------|
| `http.server.request.duration` | Histogram (seconds) | method, scheme, host, port, status, route, error.type |
| `http.server.active_requests` | UpDown Counter | method, scheme, host, port |
| `http.server.request.body.size` | Histogram (bytes) | method, scheme, host, port, status, route, error.type |
| `http.server.response.body.size` | Histogram (bytes) | method, scheme, host, port, status, route, error.type |

## Components

- **`otel_nova_stream_h`** — Cowboy stream handler for HTTP tracing and metrics
- **`otel_nova_plugin`** — Nova plugin for span enrichment with route and controller info
- **`otel_nova_prom_exporter`** — Prometheus exporter with delta-to-cumulative conversion
- **`otel_nova_prom_server`** — HTTP server for Prometheus scraping

## License

MIT
