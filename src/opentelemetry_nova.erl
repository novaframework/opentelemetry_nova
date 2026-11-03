-module(opentelemetry_nova).
-moduledoc """
OpenTelemetry instrumentation for Nova. See the README for the full setup.
""".

-export([setup/0, setup/1]).

-include_lib("opentelemetry_api/include/opentelemetry.hrl").
-include_lib("opentelemetry_api_experimental/include/otel_meter.hrl").

-doc "Creates the HTTP server metric instruments. Call it once from your application's `start/2`.".
setup() ->
    opentelemetry:get_application_tracer(?MODULE),
    ?create_histogram('http.server.request.duration', #{
        description => <<"Duration of HTTP server requests">>,
        unit => s,
        advisory_params => #{
            explicit_bucket_boundaries => [
                0.005, 0.01, 0.025, 0.05, 0.075, 0.1, 0.25, 0.5, 0.75, 1.0, 2.5, 5.0, 7.5, 10.0
            ]
        }
    }),
    ?create_updown_counter('http.server.active_requests', #{
        description => <<"Number of active HTTP server requests">>,
        unit => '{request}'
    }),
    ?create_histogram('http.server.request.body.size', #{
        description => <<"Size of HTTP server request bodies">>,
        unit => 'By'
    }),
    ?create_histogram('http.server.response.body.size', #{
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
