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

## Upgrading from 0.5

- `url.query` is available but off by default. When you set `url_query` to
  `true`, values that look secret are redacted, but the rest of the query
  string reaches your trace backend: review its retention and access rules
  first.
- A 5xx caused by a Nova crash, with `otel_nova_plugin` registered as a
  `post_request` plugin, has `error.type` set to the crash tag (`badarg`,
  `badmatch`, ...) instead of `500` on spans and metrics. A stream that ends
  without a response has the Cowboy reason (`socket_error`, ...). Alerts that
  match `error_type="500"` need updating.

## Upgrading from 0.4

These defaults and values change. Check your scrape config, dashboards and
alerts:

- `otel_nova_prom_server` binds `127.0.0.1` instead of all interfaces. A
  Prometheus that scrapes the pod IP in Kubernetes gets connection refused.
  Set `ip => {0, 0, 0, 0}` and add a `NetworkPolicy` (see
  [Prometheus metrics](#prometheus-metrics)). The listener also no longer
  accepts h2c.
- `X-Forwarded-For` and `Forwarded` are no longer trusted: `client.address` is
  the peer address unless you set `trust_forwarded_for`.
- `server.address` and `server.port` are no longer on metrics unless you set
  `metric_server_hosts`. Queries and alerts that group by them need updating.
- Methods outside the nine standard ones (`GET`, `HEAD`, `POST`, `PUT`,
  `DELETE`, `CONNECT`, `OPTIONS`, `TRACE`, `PATCH`) become `_OTHER` on spans
  and metrics. Spans keep the sent value in `http.request.method_original`.
- `http.route` values change: they are route templates (`/users/:id`,
  `/assets/[...]`) instead of raw paths, and the attribute is absent when the
  route cannot be determined.
- `otel_nova_sampler` exclusions match the method plus the raw request path
  (`~"GET /metrics"`), not the span name.

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
        {pre_request, nova_request_plugin, #{decode_json_body => true}},
        {post_request, otel_nova_plugin, #{}}
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

A route group that declares its own `plugins` does not get the global list
under Nova's default `plugin_strategy` (`local_or_global`), so its requests
lose the trace linkage. Either list `otel_nova_plugin` first in that group's
`plugins`, or, for an application mounted through `nova_apps`, set
`plugin_strategy => global_first` (Nova 0.17.0 and later) so the global
plugins run before the group's own:

```erlang
{nova_apps, [{my_admin_app, #{prefix => "/admin", plugin_strategy => global_first}}]}
```

Register `otel_nova_plugin` as the first `post_request` plugin as well, so
crashes are tagged (see [Crashes](#crashes)).

`otel_nova_plugin` never fails a request: if its instrumentation crashes it
logs `otel_nova_instrumentation_failed` without request data and lets the
request continue.

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

### 4. Options

Set these in the `opentelemetry_nova` application environment:

```erlang
{opentelemetry_nova, [
    {trust_forwarded_for, false},
    {metric_server_hosts, [~"api.example.com"]},
    {trust_incoming_context, true}
]}
```

| Option | Default | Effect |
|--------|---------|--------|
| `trust_forwarded_for` | `false` | `true`: take `client.address` from the first `X-Forwarded-For` entry, or the `Forwarded` header, when it is a valid IP address. A list of header names out of `~"forwarded"`, `~"x-forwarded-for"` and `~"x-real-ip"` uses those headers in that priority order. Otherwise `client.address` is the peer address. |
| `metric_server_hosts` | unset | Record `server.address` and `server.port` on metrics. Hosts on the list are recorded as sent (Cowboy lowercases them), others as `_OTHER`, and the port is the listener's. When unset, metrics carry neither attribute. |
| `trust_incoming_context` | `true` | Continue the trace context and baggage the client sends. Set it to `false` on a public edge: the server span then starts a new trace and links to the remote parent. `{Mod, Fun}` decides per request: `Mod:Fun(Req)` returns `true` to trust the context. Any other result, or a crash (logged), means `false`. |
| `url_query` | `false` | Set `url.query` on the span when the request has a query string, with secret-looking values redacted (see below). |
| `url_query_redact_keys` | `[]` | More key fragments whose values are replaced by `REDACTED` in `url.query`, in addition to the defaults below. Binaries, matched case-insensitively. |
| `request_headers` | `[]` | Lowercase request header names recorded as `http.request.header.<name>` span attributes. |
| `response_headers` | `[]` | Lowercase response header names recorded as `http.response.header.<name>` span attributes. |
| `span_name_formatter` | unset | `{Mod, Fun}`: the plugin names the server span `Mod:Fun(Method, Route)`. `Method` is the normalised method (`~"_OTHER"` for a non-standard one), `Route` the route template or `undefined`. A crash or a non-binary result keeps the default name and is logged as `otel_nova_instrumentation_failed`. |
| `opt_in_attributes` | `[]` | Opt-in span attributes to add: any of `'client.port'`, `'network.local.address'`, `'network.local.port'`, `'network.transport'`, `'http.request.body.size'`, `'http.response.body.size'`. |
| `sensitive_headers` | `[~"authorization", ~"proxy-authorization", ~"cookie", ~"set-cookie", ~"x-api-key"]` | Captured headers whose value is recorded as `[~"REDACTED"]`. Replaces the default list when set. |

With a list, the first listed header that the request carries decides. If
its address is not a valid IP address, `client.address` is the peer address;
later headers are not consulted. List only the header your proxy sets, for
example `[~"x-real-ip"]` behind nginx with
`proxy_set_header X-Real-IP $remote_addr`.

Enable `trust_forwarded_for` only behind a proxy that replaces these headers
with its own value. Many proxies append to `X-Forwarded-For` instead (nginx
with `$proxy_add_x_forwarded_for`, Traefik, ingress-nginx), so the first entry
is still whatever the client sent, and `client.address` stays
client-controlled.

With `url_query` enabled, `url.query` keeps the query string as sent, except
that a value becomes `REDACTED` when its key, percent-decoded and lowercased,
equals one of the semconv keys (`AWSAccessKeyId`, `Signature`, `sig`,
`X-Goog-Signature`, `X-Amz-Signature`, `X-Amz-Credential`,
`X-Amz-Security-Token`) or contains one of these fragments: `token`,
`secret`, `pass`, `pwd`, `key`, `sig`, `auth`, `session`, `sid`, `code`,
`otp`, `nonce`, `jwt`, `email`, `ticket`, `cred`, `saml`, `state`, `hash`.
Fragments from `url_query_redact_keys` are matched the same way. An entry
without `=` is replaced by `REDACTED` as a whole, since it may be a bare
token. A query string that is not valid UTF-8 is left out. Redaction is by key
name only: a secret under an innocent key, such as `?q=<api key>`, is
recorded.

Captured headers are lists of binaries, as the semantic conventions specify.
Cowboy joins a repeated request header into one value, so a request header has
one element; each `set-cookie` cookie of a response is its own element.
Response headers are read from the response Cowboy is asked to send, which
includes `date`, `server` and, for a full body, `content-length`, but not the
headers the protocol layer adds afterwards (`connection`,
`transfer-encoding`). Values that are not valid UTF-8 are left out.

With `trust_incoming_context` at its default, a client that sends a sampled
`traceparent` decides the sampling of its own request, including requests
excluded by `otel_nova_sampler` under `parent_based`, and its `baggage` is
propagated to every service the request reaches. On an edge that faces
untrusted clients, set it to `false`, or use a callback when one listener
serves both internal and public traffic. The callback runs in the connection
process before routing, on the Cowboy Req:

```erlang
-module(my_trust).
-export([internal/1]).

internal(Req) ->
    case cowboy_req:peer(Req) of
        {{10, _, _, _}, _Port} -> true;
        _ -> false
    end.
```

```erlang
{opentelemetry_nova, [{trust_incoming_context, {my_trust, internal}}]}
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
follows its parent, unless `trust_incoming_context` is `false`.

To sample the requests that are not excluded with another sampler, pass a map
with the exclusions and a sampler spec (default `always_on`). Any spec that
`otel_sampler:new/1` accepts works:

```erlang
{opentelemetry, [
    {sampler, {parent_based, #{root => {otel_nova_sampler, #{
        exclude => [~"GET /metrics", ~"GET /healthz"],
        sampler => {trace_id_ratio_based, 0.1}
    }}}}}
]}
```

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

The listener binds `127.0.0.1` and speaks HTTP/1.1 only, and `/metrics` has no
authentication. To let a scraper on another host reach it, set `ip`, for
example in Kubernetes:

```erlang
otel_nova_prom_server:child_spec(#{port => 9464, ip => {0, 0, 0, 0}})
```

and restrict who can connect with a `NetworkPolicy` that only admits the
Prometheus pods to port 9464:

```yaml
apiVersion: networking.k8s.io/v1
kind: NetworkPolicy
metadata:
  name: my-app-metrics
spec:
  podSelector:
    matchLabels:
      app: my-app
  policyTypes: [Ingress]
  ingress:
    - from:
        - namespaceSelector:
            matchLabels:
              kubernetes.io/metadata.name: monitoring
      ports:
        - port: 9464
```

A policy that selects the pods replaces the default allow-all for them, so
list your application's own port in another `ingress` rule as well.

Each metric keeps at most 2000 series. Further attribute sets are added to one
overflow series labelled `otel_metric_overflow="true"`. Label values that are
not valid UTF-8 are rendered as `_INVALID_UTF8`, so the output always parses.

`opentelemetry_nova:setup(#{prometheus => #{port => 9464}})` still works and
starts the server linked to the caller, but the supervised child is
preferred: it stops the listener with your application.

## Span Attributes

Server spans are created for every HTTP request with the following attributes:

| Attribute | Source |
|-----------|--------|
| `http.request.method` | Request method, or `_OTHER` if it is not one of the nine standard methods |
| `http.request.method_original` | Request method as sent, when it differs from `http.request.method` |
| `http.response.status_code` | Response status code |
| `http.route` | Matched route template (e.g. `/users/:id`) |
| `url.path` | Request path |
| `url.query` | Query string with secret values redacted, when present and `url_query` is enabled (see [Options](#4-options)) |
| `url.scheme` | Request scheme |
| `server.address` | Host header |
| `server.port` | Server port |
| `client.address` | Peer IP, or the `X-Forwarded-For` / `Forwarded` / `X-Real-IP` address with `trust_forwarded_for` |
| `network.peer.address` | Peer IP address |
| `network.peer.port` | Peer port |
| `network.protocol.version` | HTTP version (`1.0`, `1.1`, `2`, `3`) |
| `user_agent.original` | User-Agent header |
| `client.port` | Peer port, when `client.address` is the peer address (opt-in) |
| `network.local.address` | Listener socket address (opt-in) |
| `network.local.port` | Listener socket port (opt-in) |
| `network.transport` | `tcp`, or `udp` for HTTP/3 (opt-in) |
| `http.request.body.size` | Request body bytes read, set when the span ends (opt-in) |
| `http.response.body.size` | Response body bytes sent, set when the span ends (opt-in) |
| `http.request.header.<name>` | Request headers listed in `request_headers` |
| `http.response.header.<name>` | Response headers listed in `response_headers` |
| `error.type` | Crash tag on a Nova crash (see [Crashes](#crashes)), otherwise the status code as string on 5xx errors |
| `nova.app` | Nova application name |
| `nova.controller` | Controller module |
| `nova.action` | Action function |

Span names follow the `{METHOD} {route}` convention (e.g. `GET /users/:id`).
For an `_OTHER` method the method part is `HTTP`. Set `span_name_formatter` to
name spans differently. Keep the names low-cardinality.

`http.route` is the route template, never the raw path. Bound segments become
`:name`, and the tail matched by a catch-all route, including Nova static
routes such as `/assets/[...]`, becomes `[...]`, so every file under `/assets`
shares one label. When the template cannot be determined, `http.route` is left
out and the span keeps the name `HTTP {METHOD}`. This keeps the number of metric
series bounded by the route table.

Notes:

- `client.address` uses a forwarding header only when
  `trust_forwarded_for` is enabled, and only when the address in it is a valid
  IP address. See [Options](#4-options).
- `http.route` is taken from the `nova_route` key that newer Nova versions
  put in the Req. With older Nova versions it is rebuilt from the request path
  and the binding values. The rebuilt label is wrong for two kinds of path: a literal segment equal to a binding value (`/users/users`
  on `/users/:id` becomes `/:id/:id`), and two bindings with the same value
  (`/teams/7/users/7` on `/teams/:team/users/:user` shows one of the two names
  in both places). The labels stay bounded; only their text is wrong.
- The trace context is not detached on a WebSocket upgrade. This is harmless.
  Over HTTP/1.1 the WebSocket runs in the connection process, which never has
  the context attached. Over HTTP/2 it keeps running in the request process,
  so spans the WebSocket handler creates are children of the upgrade request's
  server span.
- With `cowboy_compress_h` after `otel_nova_stream_h`, the response body size
  is measured before compression.

## Crashes

When a controller or plugin crashes, Nova logs the crash, renders a status
page and stores `crash_info` in the Req. `otel_nova_plugin` reads it and sets
`error.type` on the server span to a low-cardinality tag: the reason when it
is an atom (`badarg`), the first element of a tagged tuple (`badmatch` for
`{badmatch, _}`), otherwise the class (`error`, `exit`, `throw`). The span
status is set to error with the tag as description, and for a 5xx response
the `error.type` metric attribute is the tag instead of the status code.

No exception event is added to the span. Nova logs the crash in the request
process, where the trace context is attached, so the log record carries the
`trace_id` and `span_id`.

What is tagged depends on where the crash happens (checked against Nova
master):

- Controller crash: Nova runs the `post_request` plugins, which tag it.
- `pre_request` plugin crash: Nova skips the `post_request` plugins and runs
  the `pre_request` plugins a second time instead. `otel_nova_plugin` tags the
  crash there.
- `throw({Status, Reason})` from a controller is a deliberate status, not a
  crash: `error.type` is the status code for a 5xx, as usual.
- Security function crash, or a crash in a `post_request` plugin listed before
  `otel_nova_plugin`: not tagged, `error.type` stays the status code.

## Metrics

| Metric | Type | Attributes |
|--------|------|------------|
| `http.server.request.duration` | Histogram (seconds) | method, scheme, status, route, error.type, added attributes |
| `http.server.active_requests` | UpDown Counter (Prometheus gauge of in-flight requests) | method, scheme |
| `http.server.request.body.size` | Histogram (bytes) | method, scheme, status, route, error.type, added attributes |
| `http.server.response.body.size` | Histogram (bytes) | method, scheme, status, route, error.type, added attributes |

Every attribute value is bounded: the method is one of the nine standard
methods or `_OTHER`, the scheme is `http`, `https` or `_OTHER`, and the route
is a template. `server.address` and `server.port` are added only when
`metric_server_hosts` is set.

### Per-request metric attributes

A controller or plugin can add attributes to the duration and body size
measurements of its request:

```erlang
show(Req = #{bindings := #{~"tenant" := Tenant}}) ->
    ok = opentelemetry_nova:add_metric_attributes(Req, #{tenant_tier => tier(Tenant)}),
    {json, #{}}.
```

Later calls are merged in, a later value winning. The built-in attributes
(`http.request.method`, `url.scheme`, `http.route`,
`http.response.status_code`, `error.type`, `server.address`, `server.port`)
cannot be overridden, invalid attribute values are dropped, and
`http.server.active_requests` does not get them. The call does nothing when
the request does not pass through `otel_nova_stream_h`.

Every distinct value becomes a separate series in every one of these
metrics. Keeping the values bounded is up to you: use a tier or a plan, never
a user, tenant or request id.

## Components

- **`otel_nova_stream_h`**: Cowboy stream handler for the server span and the HTTP metrics
- **`otel_nova_plugin`**: Nova plugin that attaches the trace context in the request process and adds route and controller info
- **`otel_nova_prom_exporter`**: metric exporter rendering the Prometheus text format
- **`otel_nova_prom_server`**: HTTP listener for Prometheus scraping
- **`otel_nova_sampler`**: sampler that drops server spans of excluded requests

## License

MIT
