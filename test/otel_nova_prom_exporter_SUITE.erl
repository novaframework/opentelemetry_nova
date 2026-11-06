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
    [
        format_histogram,
        format_counter,
        format_gauge,
        format_updown_counter,
        label_dots_to_underscores,
        label_escaping,
        invalid_utf8_label_value,
        delta_series_overflow,
        cumulative_series_overflow,
        cumulative_histogram_series_overflow,
        overflow_attrs_do_not_consume_series,
        meta_row_stores_no_datapoints,
        second_exporter_instance_is_ignored,
        unit_suffix_seconds,
        unit_suffix_bytes,
        empty_metrics,
        delta_accumulation,
        delta_updown_counter_accumulates,
        cumulative_counter_replaces,
        cumulative_histogram_replaces,
        reader_api_export_3,
        http_endpoint
    ].

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

    ?assertMatch(
        {match, _}, re:run(Output, <<"# TYPE http_server_request_duration_seconds histogram">>)
    ),
    ?assertMatch(
        {match, _},
        re:run(
            Output,
            <<"http_server_request_duration_seconds_bucket\\{http_request_method=\"GET\",le=\"0.01\"\\} 1">>
        )
    ),
    ?assertMatch(
        {match, _},
        re:run(
            Output,
            <<"http_server_request_duration_seconds_bucket\\{http_request_method=\"GET\",le=\"0.05\"\\} 2">>
        )
    ),
    ?assertMatch(
        {match, _},
        re:run(
            Output,
            <<"http_server_request_duration_seconds_bucket\\{http_request_method=\"GET\",le=\"0.1\"\\} 3">>
        )
    ),
    ?assertMatch(
        {match, _},
        re:run(
            Output,
            <<"http_server_request_duration_seconds_bucket\\{http_request_method=\"GET\",le=\"\\+Inf\"\\} 3">>
        )
    ),
    ?assertMatch(
        {match, _},
        re:run(
            Output,
            <<"http_server_request_duration_seconds_sum\\{http_request_method=\"GET\"\\} 0.15">>
        )
    ),
    ?assertMatch(
        {match, _},
        re:run(
            Output,
            <<"http_server_request_duration_seconds_count\\{http_request_method=\"GET\"\\} 3">>
        )
    ),
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
    ?assertMatch(
        {match, _},
        re:run(Output, <<"http_server_requests_total\\{http_request_method=\"GET\"\\} 42">>)
    ),
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
    ?assertMatch(
        {match, _},
        re:run(Output, <<"http_server_active_requests\\{http_request_method=\"GET\"\\} 5">>)
    ),
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

invalid_utf8_label_value(_Config) ->
    Metric = sum_metric(hits, temporality_delta, true, [{#{method => <<"G", 16#FF, "T">>}, 1}]),
    ok = otel_nova_prom_exporter:export([Metric], undefined, #{}),
    Output = otel_nova_prom_exporter:get_metrics(),
    ?assertEqual(Output, unicode:characters_to_binary(Output)),
    ?assertMatch({match, _}, re:run(Output, ~"hits_total\\{method=\"_INVALID_UTF8\"\\} 1\n")).

delta_series_overflow(_Config) ->
    Datapoints = [{#{n => N}, 1} || N <- lists:seq(1, 2500)],
    ok = otel_nova_prom_exporter:export(
        [sum_metric(hits, temporality_delta, true, Datapoints)], undefined, #{}
    ),
    ok = otel_nova_prom_exporter:export(
        [sum_metric(hits, temporality_delta, true, [{#{n => 9999}, 1}])], undefined, #{}
    ),
    Output = otel_nova_prom_exporter:get_metrics(),
    ?assertEqual(2001, length(binary:matches(Output, ~"hits_total{"))),
    ?assertMatch(
        {match, _}, re:run(Output, ~"hits_total\\{otel_metric_overflow=\"true\"\\} 501\n")
    ).

cumulative_series_overflow(_Config) ->
    Datapoints = [{#{n => N}, 2} || N <- lists:seq(1, 2100)],
    Metric = sum_metric(hits, temporality_cumulative, true, Datapoints),
    ok = otel_nova_prom_exporter:export([Metric], undefined, #{}),
    ok = otel_nova_prom_exporter:export([Metric], undefined, #{}),
    Output = otel_nova_prom_exporter:get_metrics(),
    ?assertEqual(2001, length(binary:matches(Output, ~"hits_total{"))),
    ?assertMatch(
        {match, _}, re:run(Output, ~"hits_total\\{otel_metric_overflow=\"true\"\\} 200\n")
    ).

cumulative_histogram_series_overflow(_Config) ->
    Datapoints = [hist_datapoint(#{n => N}, 2, [1, 1]) || N <- lists:seq(1, 2003)],
    Metric = #metric{
        name = latency,
        description = ~"test",
        unit = undefined,
        data = #histogram{datapoints = Datapoints, aggregation_temporality = temporality_cumulative}
    },
    ok = otel_nova_prom_exporter:export([Metric], undefined, #{}),
    ok = otel_nova_prom_exporter:export([Metric], undefined, #{}),
    Output = otel_nova_prom_exporter:get_metrics(),
    ?assertEqual(2001, length(binary:matches(Output, ~"latency_count{"))),
    ?assertMatch(
        {match, _}, re:run(Output, ~"latency_count\\{otel_metric_overflow=\"true\"\\} 6\n")
    ),
    ?assertMatch(
        {match, _},
        re:run(Output, ~"latency_bucket\\{otel_metric_overflow=\"true\",le=\"1.0\"\\} 3\n")
    ).

overflow_attrs_do_not_consume_series(_Config) ->
    Overflow = sum_metric(hits, temporality_cumulative, true, [
        {#{'otel.metric.overflow' => true}, 1}
    ]),
    [ok = otel_nova_prom_exporter:export([Overflow], undefined, #{}) || _ <- lists:seq(1, 5)],
    Datapoints = [{#{n => N}, 1} || N <- lists:seq(1, 2000)],
    ok = otel_nova_prom_exporter:export(
        [sum_metric(hits, temporality_cumulative, true, Datapoints)], undefined, #{}
    ),
    ?assertEqual(
        2000, length(binary:matches(otel_nova_prom_exporter:get_metrics(), ~"hits_total{n="))
    ).

meta_row_stores_no_datapoints(_Config) ->
    ok = otel_nova_prom_exporter:export([counter(temporality_delta, 1)], undefined, #{}),
    ?assertMatch(
        [{{meta, ~"hits_total"}, ~"test", ~"counter"}],
        ets:lookup(otel_nova_prom_metrics, {meta, ~"hits_total"})
    ).

second_exporter_instance_is_ignored(_Config) ->
    Self = self(),
    {Pid, Ref} = spawn_monitor(fun() -> Self ! {init, otel_nova_prom_exporter:init(#{})} end),
    receive
        {init, Result} -> ?assertEqual(ignore, Result)
    after 1000 ->
        ct:fail(no_init_result)
    end,
    receive
        {'DOWN', Ref, process, Pid, _} -> ok
    end,
    ?assertEqual(Self, ets:info(otel_nova_prom_metrics, owner)),
    ok = otel_nova_prom_exporter:export([counter(temporality_delta, 1)], undefined, #{}),
    ?assertMatch({match, _}, re:run(otel_nova_prom_exporter:get_metrics(), ~"hits_total 1\n")).

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
                #datapoint{
                    attributes = #{key => <<"a">>},
                    start_time = 0,
                    time = 1000,
                    value = 10,
                    exemplars = [],
                    flags = 0
                }
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
                #datapoint{
                    attributes = #{key => <<"a">>},
                    start_time = 1000,
                    time = 2000,
                    value = 7,
                    exemplars = [],
                    flags = 0
                }
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

delta_updown_counter_accumulates(_Config) ->
    otel_nova_prom_exporter:export([updown(temporality_delta, 1)], undefined, #{}),
    otel_nova_prom_exporter:export([updown(temporality_delta, 1)], undefined, #{}),
    ?assertMatch({match, _}, re:run(otel_nova_prom_exporter:get_metrics(), ~"active 2\n")),
    otel_nova_prom_exporter:export([updown(temporality_delta, -2)], undefined, #{}),
    ?assertMatch({match, _}, re:run(otel_nova_prom_exporter:get_metrics(), ~"active 0\n")),
    ok.

cumulative_counter_replaces(_Config) ->
    otel_nova_prom_exporter:export([counter(temporality_cumulative, 10)], undefined, #{}),
    otel_nova_prom_exporter:export([counter(temporality_cumulative, 12)], undefined, #{}),
    ?assertMatch({match, _}, re:run(otel_nova_prom_exporter:get_metrics(), ~"hits_total 12\n")),
    ok.

cumulative_histogram_replaces(_Config) ->
    otel_nova_prom_exporter:export([histogram(temporality_cumulative, 2)], undefined, #{}),
    otel_nova_prom_exporter:export([histogram(temporality_cumulative, 3)], undefined, #{}),
    Output = otel_nova_prom_exporter:get_metrics(),
    ?assertMatch({match, _}, re:run(Output, ~"latency_count 3\n")),
    ?assertMatch({match, _}, re:run(Output, ~"latency_bucket\\{le=\"\\+Inf\"\\} 3\n")),
    ok.

reader_api_export_3(_Config) ->
    ?assertEqual(
        ok, otel_nova_prom_exporter:export([counter(temporality_delta, 4)], undefined, #{})
    ),
    ?assertMatch({match, _}, re:run(otel_nova_prom_exporter:get_metrics(), ~"hits_total 4\n")),
    ?assertEqual(ok, otel_nova_prom_exporter:shutdown(#{})),
    ?assertEqual(<<>>, otel_nova_prom_exporter:get_metrics()),
    {ok, _} = otel_nova_prom_exporter:init(#{}),
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

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

updown(Temporality, Value) ->
    sum_metric(active, Temporality, false, Value).

counter(Temporality, Value) ->
    sum_metric(hits, Temporality, true, Value).

sum_metric(Name, Temporality, Monotonic, Datapoints) when is_list(Datapoints) ->
    #metric{
        name = Name,
        description = ~"test",
        unit = undefined,
        data = #sum{
            datapoints = [
                #datapoint{
                    attributes = Attrs,
                    start_time = 0,
                    time = 1000,
                    value = Value,
                    exemplars = [],
                    flags = 0
                }
             || {Attrs, Value} <- Datapoints
            ],
            aggregation_temporality = Temporality,
            is_monotonic = Monotonic
        }
    };
sum_metric(Name, Temporality, Monotonic, Value) ->
    #metric{
        name = Name,
        description = ~"test",
        unit = undefined,
        data = #sum{
            datapoints = [
                #datapoint{
                    attributes = #{},
                    start_time = 0,
                    time = 1000,
                    value = Value,
                    exemplars = [],
                    flags = 0
                }
            ],
            aggregation_temporality = Temporality,
            is_monotonic = Monotonic
        }
    }.

hist_datapoint(Attrs, Count, BucketCounts) ->
    #histogram_datapoint{
        attributes = Attrs,
        start_time = 0,
        time = 1000,
        count = Count,
        sum = Count,
        bucket_counts = BucketCounts,
        explicit_bounds = [1.0],
        exemplars = [],
        flags = 0,
        min = 1,
        max = 1
    }.

histogram(Temporality, Count) ->
    #metric{
        name = latency,
        description = ~"test",
        unit = undefined,
        data = #histogram{
            datapoints = [
                #histogram_datapoint{
                    attributes = #{},
                    start_time = 0,
                    time = 1000,
                    count = Count,
                    sum = Count,
                    bucket_counts = [Count, 0],
                    explicit_bounds = [1.0],
                    exemplars = [],
                    flags = 0,
                    min = 1,
                    max = 1
                }
            ],
            aggregation_temporality = Temporality
        }
    }.
