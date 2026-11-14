-module(opentelemetry_nova).
-moduledoc """
OpenTelemetry instrumentation for Nova. See the README for the full setup.
""".

-export([setup/0, setup/1, add_metric_attributes/2, set_stream_duration/2]).

-doc "Creates the HTTP server metric instruments. Call it once from your application's `start/2`.".
-spec setup() -> ok.
setup() ->
    opentelemetry:get_application_tracer(?MODULE),
    Meter = meter(),
    otel_meter:create_histogram(Meter, 'http.server.request.duration', #{
        description => <<"Duration of HTTP server requests">>,
        unit => s,
        advisory_params => #{
            explicit_bucket_boundaries => [
                0.005, 0.01, 0.025, 0.05, 0.075, 0.1, 0.25, 0.5, 0.75, 1.0, 2.5, 5.0, 7.5, 10.0
            ]
        }
    }),
    otel_meter:create_updown_counter(Meter, 'http.server.active_requests', #{
        description => <<"Number of active HTTP server requests">>,
        unit => '{request}'
    }),
    otel_meter:create_histogram(Meter, 'http.server.request.body.size', #{
        description => <<"Size of HTTP server request bodies">>,
        unit => 'By'
    }),
    otel_meter:create_histogram(Meter, 'http.server.response.body.size', #{
        description => <<"Size of HTTP server response bodies">>,
        unit => 'By'
    }),
    ok.

-doc """
Same as `setup/0`. With `#{prometheus => #{port => Port}}` it also starts
`otel_nova_prom_server` linked to the caller. Prefer adding
`otel_nova_prom_server:child_spec/1` to your supervisor instead, so the
listener is stopped with your application.
""".
setup(Opts) ->
    setup(),
    case maps:get(prometheus, Opts, undefined) of
        undefined ->
            ok;
        PromOpts when is_map(PromOpts) ->
            {ok, _} = otel_nova_prom_server:start_link(PromOpts),
            ok
    end.

-doc """
Adds attributes to the `http.server.request.duration`,
`http.server.request.body.size` and `http.server.response.body.size`
measurements of the current request. Call it from a controller or plugin with
the Req of the request; later calls are merged in, a later value winning.

Keys are atoms or binaries matching `^[a-zA-Z_][a-zA-Z0-9_.]*$` and are
recorded as binaries. Values are atoms, booleans, integers or UTF-8 binaries
of at most 64 bytes. A key whose Prometheus label form (dots become
underscores) is `le`, a built-in label (`http_route`, `error_type`, ...) or
the label of another added key is dropped, as is any invalid entry. At most
10 keys are kept per request.

Every distinct value becomes a separate metric series, so only use
attributes with a small, bounded set of values.

Does nothing when the request is not instrumented by `otel_nova_stream_h`.
""".
-spec add_metric_attributes(cowboy_req:req(), opentelemetry:attributes_map()) -> ok.
add_metric_attributes(Req = #{otel_nova_ctx := _}, Attrs) when is_map(Attrs) ->
    cowboy_req:cast({otel_nova_metric_attributes, Attrs}, Req);
add_metric_attributes(_Req, Attrs) when is_map(Attrs) ->
    ok.

-doc """
Sets how `http.server.request.duration` measures the current request, for a
response that stays open long after it started: server-sent events, a chunked
download, long polling. Call it from a controller or plugin with the Req of
the request, before the response starts.

* `first_byte`: the time until the response headers, or a complete response,
  are handed to Cowboy.
* `exclude`: no duration is recorded.
* `full`: the time until the stream ends, which is the default for every
  request.

It overrides the `stream_duration` option, which applies to
`text/event-stream` responses only. The server span, the body size histograms
and `http.server.active_requests` are not affected.

Does nothing when the request is not instrumented by `otel_nova_stream_h`.
""".
-spec set_stream_duration(cowboy_req:req(), full | first_byte | exclude) -> ok.
set_stream_duration(Req = #{otel_nova_ctx := _}, Mode) when
    Mode =:= full; Mode =:= first_byte; Mode =:= exclude
->
    cowboy_req:cast({otel_nova_stream_duration, Mode}, Req);
set_stream_duration(_Req, Mode) when Mode =:= full; Mode =:= first_byte; Mode =:= exclude ->
    ok.

-spec meter() -> opentelemetry_experimental:meter().
meter() ->
    case opentelemetry:get_application_scope(?MODULE) of
        undefined -> opentelemetry_experimental:get_meter();
        Scope -> opentelemetry_experimental:get_meter(Scope)
    end.
