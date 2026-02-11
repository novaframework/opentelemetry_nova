-module(otel_nova_prom_exporter_SUITE).
-behaviour(ct_suite).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include_lib("opentelemetry_experimental/include/otel_metrics.hrl").

%%--------------------------------------------------------------------
%% CT callbacks
%%--------------------------------------------------------------------

all() ->
    [format_histogram,
     format_counter,
     format_gauge,
     format_updown_counter,
     label_dots_to_underscores,
     label_escaping,
     unit_suffix_seconds,
     unit_suffix_bytes,
     empty_metrics,
     delta_accumulation,
     http_endpoint].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(http_endpoint, Config) ->
    application:ensure_all_started(cowboy),
    {ok, _} = otel_nova_prom_exporter:init(#{}),
    {ok, _} = otel_nova_prom_server:start_link(#{port => 19464}),
    Config;
init_per_testcase(_TestCase, Config) ->
    {ok, _} = otel_nova_prom_exporter:init(#{}),
    Config.

end_per_testcase(http_endpoint, _Config) ->
    otel_nova_prom_server:stop(),
    otel_nova_prom_exporter:shutdown(),
    ok;
end_per_testcase(_TestCase, _Config) ->
    otel_nova_prom_exporter:shutdown(),
    ok.

%%--------------------------------------------------------------------
%% Tests
%%--------------------------------------------------------------------

format_histogram(_Config) ->
    Metric = #metric{
        name = 'http.server.request.duration',
        description = <<"Duration of HTTP server requests">>,
        unit = s,
        data = #histogram{
            datapoints = [
                #histogram_datapoint{
                    attributes = #{'http.request.method' => <<"GET">>},
                    start_time = 0,
                    time = 1000,
                    count = 3,
                    sum = 0.15,
                    bucket_counts = [1, 1, 1, 0],
                    explicit_bounds = [0.01, 0.05, 0.1],
                    exemplars = [],
                    flags = 0,
                    min = 0.005,
                    max = 0.1
                }
            ],
            aggregation_temporality = temporality_cumulative
        }
    },
    otel_nova_prom_exporter:export(metrics, [Metric], undefined, #{}),
    Output = otel_nova_prom_exporter:get_metrics(),

    ?assertMatch({match, _}, re:run(Output, <<"# TYPE http_server_request_duration_seconds histogram">>)),
    ?assertMatch({match, _}, re:run(Output, <<"http_server_request_duration_seconds_bucket\\{http_request_method=\"GET\",le=\"0.01\"\\} 1">>)),
    ?assertMatch({match, _}, re:run(Output, <<"http_server_request_duration_seconds_bucket\\{http_request_method=\"GET\",le=\"0.05\"\\} 2">>)),
    ?assertMatch({match, _}, re:run(Output, <<"http_server_request_duration_seconds_bucket\\{http_request_method=\"GET\",le=\"0.1\"\\} 3">>)),
    ?assertMatch({match, _}, re:run(Output, <<"http_server_request_duration_seconds_bucket\\{http_request_method=\"GET\",le=\"\\+Inf\"\\} 3">>)),
    ?assertMatch({match, _}, re:run(Output, <<"http_server_request_duration_seconds_sum\\{http_request_method=\"GET\"\\} 0.15">>)),
    ?assertMatch({match, _}, re:run(Output, <<"http_server_request_duration_seconds_count\\{http_request_method=\"GET\"\\} 3">>)),
    ok.

format_counter(_Config) ->
    Metric = #metric{
        name = 'http.server.requests',
        description = <<"Total HTTP requests">>,
        unit = undefined,
        data = #sum{
            datapoints = [
                #datapoint{
                    attributes = #{'http.request.method' => <<"GET">>},
                    start_time = 0,
                    time = 1000,
                    value = 42,
                    exemplars = [],
                    flags = 0
                }
            ],
            aggregation_temporality = temporality_cumulative,
            is_monotonic = true
        }
    },
    otel_nova_prom_exporter:export(metrics, [Metric], undefined, #{}),
    Output = otel_nova_prom_exporter:get_metrics(),

    ?assertMatch({match, _}, re:run(Output, <<"# TYPE http_server_requests_total counter">>)),
    ?assertMatch({match, _}, re:run(Output, <<"http_server_requests_total\\{http_request_method=\"GET\"\\} 42">>)),
    ok.

format_gauge(_Config) ->
    Metric = #metric{
        name = 'system.cpu.utilization',
        description = <<"CPU utilization">>,
        unit = undefined,
        data = #gauge{
            datapoints = [
                #datapoint{
                    attributes = #{},
                    start_time = 0,
                    time = 1000,
                    value = 0.75,
                    exemplars = [],
                    flags = 0
                }
            ]
        }
    },
    otel_nova_prom_exporter:export(metrics, [Metric], undefined, #{}),
    Output = otel_nova_prom_exporter:get_metrics(),

    ?assertMatch({match, _}, re:run(Output, <<"# TYPE system_cpu_utilization gauge">>)),
    ?assertMatch({match, _}, re:run(Output, <<"system_cpu_utilization 0.75">>)),
    ok.

format_updown_counter(_Config) ->
    Metric = #metric{
        name = 'http.server.active_requests',
        description = <<"Active requests">>,
        unit = undefined,
        data = #sum{
            datapoints = [
                #datapoint{
                    attributes = #{'http.request.method' => <<"GET">>},
                    start_time = 0,
                    time = 1000,
                    value = 5,
                    exemplars = [],
                    flags = 0
                }
            ],
            aggregation_temporality = temporality_cumulative,
            is_monotonic = false
        }
    },
    otel_nova_prom_exporter:export(metrics, [Metric], undefined, #{}),
    Output = otel_nova_prom_exporter:get_metrics(),

    ?assertMatch({match, _}, re:run(Output, <<"# TYPE http_server_active_requests gauge">>)),
    ?assertMatch({match, _}, re:run(Output, <<"http_server_active_requests\\{http_request_method=\"GET\"\\} 5">>)),
    ok.

label_dots_to_underscores(_Config) ->
    Metric = #metric{
        name = 'test.metric',
        description = <<"Test">>,
        unit = undefined,
        data = #gauge{
            datapoints = [
                #datapoint{
                    attributes = #{'label.with.dots' => <<"value">>},
                    start_time = 0,
                    time = 1000,
                    value = 1,
                    exemplars = [],
                    flags = 0
                }
            ]
        }
    },
    otel_nova_prom_exporter:export(metrics, [Metric], undefined, #{}),
    Output = otel_nova_prom_exporter:get_metrics(),

    ?assertMatch({match, _}, re:run(Output, <<"test_metric\\{label_with_dots=\"value\"\\}">>)),
    ok.

label_escaping(_Config) ->
    Metric = #metric{
        name = 'test.escape',
        description = <<"Test escaping">>,
        unit = undefined,
        data = #gauge{
            datapoints = [
                #datapoint{
                    attributes = #{path => <<"value with \"quotes\" and \\backslash">>},
                    start_time = 0,
                    time = 1000,
                    value = 1,
                    exemplars = [],
                    flags = 0
                }
            ]
        }
    },
    otel_nova_prom_exporter:export(metrics, [Metric], undefined, #{}),
    Output = otel_nova_prom_exporter:get_metrics(),

    ?assertMatch({match, _}, re:run(Output, <<"\\\\\"quotes\\\\\"">>)),
    ?assertMatch({match, _}, re:run(Output, <<"\\\\\\\\">>)),
    ok.

unit_suffix_seconds(_Config) ->
    Metric = #metric{
        name = 'http.server.request.duration',
        description = <<"Duration">>,
        unit = s,
        data = #histogram{
            datapoints = [
                #histogram_datapoint{
                    attributes = #{},
                    start_time = 0,
                    time = 1000,
                    count = 1,
                    sum = 0.5,
                    bucket_counts = [1, 0],
                    explicit_bounds = [1.0],
                    exemplars = [],
                    flags = 0,
                    min = 0.5,
                    max = 0.5
                }
            ],
            aggregation_temporality = temporality_cumulative
        }
    },
    otel_nova_prom_exporter:export(metrics, [Metric], undefined, #{}),
    Output = otel_nova_prom_exporter:get_metrics(),

    ?assertMatch({match, _}, re:run(Output, <<"http_server_request_duration_seconds">>)),
    ok.

unit_suffix_bytes(_Config) ->
    Metric = #metric{
        name = 'http.server.request.body.size',
        description = <<"Body size">>,
        unit = 'By',
        data = #histogram{
            datapoints = [
                #histogram_datapoint{
                    attributes = #{},
                    start_time = 0,
                    time = 1000,
                    count = 1,
                    sum = 100,
                    bucket_counts = [1, 0],
                    explicit_bounds = [1000.0],
                    exemplars = [],
                    flags = 0,
                    min = 100,
                    max = 100
                }
            ],
            aggregation_temporality = temporality_cumulative
        }
    },
    otel_nova_prom_exporter:export(metrics, [Metric], undefined, #{}),
    Output = otel_nova_prom_exporter:get_metrics(),

    ?assertMatch({match, _}, re:run(Output, <<"http_server_request_body_size_bytes">>)),
    ok.

delta_accumulation(_Config) ->
    %% First delta export
    Metric1 = #metric{
        name = 'test.counter',
        description = <<"Test counter">>,
        unit = undefined,
        data = #sum{
            datapoints = [
                #datapoint{attributes = #{key => <<"a">>},
                           start_time = 0, time = 1000,
                           value = 10, exemplars = [], flags = 0}
            ],
            aggregation_temporality = temporality_delta,
            is_monotonic = true
        }
    },
    otel_nova_prom_exporter:export(metrics, [Metric1], undefined, #{}),

    %% Second delta export — should accumulate
    Metric2 = #metric{
        name = 'test.counter',
        description = <<"Test counter">>,
        unit = undefined,
        data = #sum{
            datapoints = [
                #datapoint{attributes = #{key => <<"a">>},
                           start_time = 1000, time = 2000,
                           value = 7, exemplars = [], flags = 0}
            ],
            aggregation_temporality = temporality_delta,
            is_monotonic = true
        }
    },
    otel_nova_prom_exporter:export(metrics, [Metric2], undefined, #{}),

    Output = otel_nova_prom_exporter:get_metrics(),
    %% Should show cumulative value: 10 + 7 = 17
    ?assertMatch({match, _}, re:run(Output, <<"test_counter_total\\{key=\"a\"\\} 17">>)),
    ok.

empty_metrics(_Config) ->
    otel_nova_prom_exporter:export(metrics, [], undefined, #{}),
    Output = otel_nova_prom_exporter:get_metrics(),
    ?assertEqual(<<>>, Output),
    ok.

http_endpoint(_Config) ->
    %% Export some metrics first
    Metric = #metric{
        name = 'test.http',
        description = <<"Test">>,
        unit = undefined,
        data = #gauge{
            datapoints = [
                #datapoint{
                    attributes = #{},
                    start_time = 0,
                    time = 1000,
                    value = 42,
                    exemplars = [],
                    flags = 0
                }
            ]
        }
    },
    otel_nova_prom_exporter:export(metrics, [Metric], undefined, #{}),

    {ok, {{_, 200, _}, Headers, Body}} =
        httpc:request(get, {"http://localhost:19464/metrics", []}, [], []),

    ContentType = proplists:get_value("content-type", Headers),
    ?assertMatch("text/plain" ++ _, ContentType),
    ?assertMatch({match, _}, re:run(Body, "test_http 42")),
    ok.
