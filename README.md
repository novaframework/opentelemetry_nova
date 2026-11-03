# opentelemetry_nova

OpenTelemetry instrumentation for the [Nova](https://github.com/novaframework/nova) web framework.

Automatic HTTP request tracing and metrics following the [OTel HTTP semantic conventions](https://opentelemetry.io/docs/specs/semconv/http/).

## Installation

Add the library, the OpenTelemetry SDK and an exporter to your dependencies in `rebar.config`:

```erlang
{deps, [
    {opentelemetry_nova, "~> 0.4"},
    {opentelemetry, "~> 1.7"},
    {opentelemetry_experimental, "~> 0.6"},
    {opentelemetry_exporter, "~> 1.11"}
]}.
```

List `opentelemetry_exporter`, `opentelemetry_experimental` and `opentelemetry`
before your own application in the release (or in `applications` of your
`.app.src`) so the SDK is running before the first request.

## Setup

Tracing needs three pieces: the stream handler, the plugin and a call to
`setup/0`.

### 1. Stream handler and plugin

```erlang
{nova, [
    {cowboy_configuration, #{
        port => 8080,
        stream_handlers => [
            otel_nova_stream_h,
            nova_stream_h,
            cowboy_compress_h,
            cowboy_stream_h
        ]
    }},
    {plugins, [
        {pre_request, otel_nova_plugin, #{}},
        {pre_request, nova_request_plugin, #{decode_json_body => true}}
    ]}
]}
```

`otel_nova_stream_h` must come first among the stream handlers. It starts the
server span, records the HTTP metrics and ends the span when the stream
terminates.

`otel_nova_plugin` must be the first `pre_request` plugin. Cowboy runs stream
handlers in the connection process and Nova runs plugins and controllers in a
separate request process. The stream handler passes the trace context to the
request process in the Req, and the plugin attaches it there, so every span
created afterwards (in later plugins, security functions, controllers, or by
libraries such as `opentelemetry_kura`) is a child of the server span. The
plugin also names the span after the route, adds the `nova.*` attributes, puts
`trace_id` and `span_id` in the logger process metadata and reports the route
for the metrics. Plugins listed before it run without a trace context.

### 2. Instruments

Call `setup/0` from your application's `start/2` to create the metric
instruments:

```erlang
start(_Type, _Args) ->
    ok = opentelemetry_nova:setup(),
    my_app_sup:start_link().
```

### 3. SDK configuration

Traces, exported over OTLP:

```erlang
{opentelemetry, [
    {span_processor, batch},
    {traces_exporter, otlp}
]},
{opentelemetry_exporter, [
    {otlp_protocol, http_protobuf},
    {otlp_endpoint, "http://localhost:4318"}
]}
```

## Excluding requests from tracing

`otel_nova_sampler` drops the server span of excluded requests:

```erlang
{opentelemetry, [
    {sampler, {parent_based, #{root => {otel_nova_sampler, [~"GET /metrics", ~"GET /healthz"]}}}}
]}
```

The sampler decides when the server span starts, before Nova has routed the
request. Entries are `"METHOD /path"` and match the raw request path (without
the query string), not the route template, so `~"GET /users/:id"` does not match
`/users/42`. With `parent_based`, a request carrying a sampled `traceparent`
follows its parent.

## Prometheus metrics

Configure a metric reader that exports to `otel_nova_prom_exporter`:

```erlang
{opentelemetry_experimental, [
    {readers, [
        #{
            module => otel_metric_reader,
            config => #{
                export_interval_ms => 10000,
                exporter => {otel_nova_prom_exporter, #{}}
            }
        }
    ]}
]}
```

Then serve `/metrics` from your supervision tree:

```erlang
init([]) ->
    Children = [otel_nova_prom_server:child_spec(#{port => 9464})],
    {ok, {#{strategy => one_for_one}, Children}}.
```

The endpoint shows the values of the last reader collection, so it lags by at
most `export_interval_ms`. The exporter turns delta datapoints into cumulative
values and stores cumulative datapoints as they are, so it is correct for either
reader temporality.

`opentelemetry_nova:setup(#{prometheus => #{port => 9464}})` still works and
starts the server linked to the caller, but the supervised child is
preferred: it stops the listener with your application.

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
| `http.server.active_requests` | UpDown Counter (Prometheus gauge of in-flight requests) | method, scheme, host, port |
| `http.server.request.body.size` | Histogram (bytes) | method, scheme, host, port, status, route, error.type |
| `http.server.response.body.size` | Histogram (bytes) | method, scheme, host, port, status, route, error.type |

## Components

- **`otel_nova_stream_h`**: Cowboy stream handler for the server span and the HTTP metrics
- **`otel_nova_plugin`**: Nova plugin that attaches the trace context in the request process and adds route and controller info
- **`otel_nova_prom_exporter`**: metric exporter rendering the Prometheus text format
- **`otel_nova_prom_server`**: HTTP listener for Prometheus scraping
- **`otel_nova_sampler`**: sampler that drops server spans of excluded requests

## License

MIT
