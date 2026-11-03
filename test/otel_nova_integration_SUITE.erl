-module(otel_nova_integration_SUITE).
-behaviour(ct_suite).

-export([all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).
-export([log/2]).
-export([
    controller_span_is_child_of_server_span/1,
    sendfile_response_is_served_and_measured/1,
    sendfile_response_logs_no_request_data/1,
    instrumentation_failure_logs_no_request_data/1,
    prometheus_endpoint_serves_recorded_metrics/1,
    active_requests_gauge_tracks_in_flight_requests/1,
    supervised_prom_server_stops_listener_with_app/1,
    setup_prom_server_survives_app_restart/1
]).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include_lib("opentelemetry/include/otel_span.hrl").

-define(HTTP_PORT, 58607).
-define(METRICS_PORT, 58617).
-define(LISTENER, otel_nova_integration_http).
-define(COOKIE, "session=s3cr3t-cookie-value").

all() ->
    [
        controller_span_is_child_of_server_span,
        sendfile_response_is_served_and_measured,
        sendfile_response_logs_no_request_data,
        instrumentation_failure_logs_no_request_data,
        prometheus_endpoint_serves_recorded_metrics,
        active_requests_gauge_tracks_in_flight_requests,
        supervised_prom_server_stops_listener_with_app,
        setup_prom_server_survives_app_restart
    ].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started([inets, cowboy, opentelemetry]),
    ok = application:set_env(opentelemetry_experimental, readers, [
        #{module => otel_metric_reader, config => #{exporter => {otel_nova_prom_exporter, #{}}}}
    ]),
    {ok, _} = application:ensure_all_started(opentelemetry_experimental),
    ok = opentelemetry_nova:setup(),
    {ok, PluginManager} = nova_plugin_manager:start_link(),
    unlink(PluginManager),

    StaticFile = filename:join(?config(priv_dir, Config), "static.txt"),
    StaticBody = binary:copy(~"static file contents\n", 100),
    ok = file:write_file(StaticFile, StaticBody),
    {ok, _} = cowboy:start_clear(?LISTENER, [{port, ?HTTP_PORT}], #{
        stream_handlers => [otel_nova_stream_h, cowboy_stream_h],
        middlewares => [otel_nova_test_router, nova_plugin_handler, otel_nova_test_handler],
        env => #{static_file => {StaticFile, byte_size(StaticBody)}}
    }),
    [{plugin_manager, PluginManager}, {static_body, StaticBody} | Config].

end_per_suite(Config) ->
    ok = cowboy:stop_listener(?LISTENER),
    gen_server:stop(?config(plugin_manager, Config)),
    application:stop(opentelemetry_experimental),
    application:stop(opentelemetry),
    ok.

init_per_testcase(TestCase, Config) when
    TestCase =:= sendfile_response_logs_no_request_data;
    TestCase =:= instrumentation_failure_logs_no_request_data
->
    ok = logger:add_handler(?MODULE, ?MODULE, #{config => #{pid => self()}}),
    init_per_testcase(default, Config);
init_per_testcase(_TestCase, Config) ->
    otel_batch_processor:set_exporter(otel_exporter_pid, self()),
    Config.

end_per_testcase(_TestCase, _Config) ->
    _ = logger:remove_handler(?MODULE),
    _ = application:stop(otel_nova_test_app),
    _ = application:unload(otel_nova_test_app),
    otel_ctx:clear(),
    ok.

%% logger handler callback
log(Event, #{config := #{pid := Pid}}) ->
    Pid ! {log_event, Event}.

%%--------------------------------------------------------------------
%% Tests
%%--------------------------------------------------------------------

controller_span_is_child_of_server_span(_Config) ->
    {200, _} = get("/child", []),
    Spans = receive_spans(2),
    [Server] = [S || S = #span{kind = server} <- Spans],
    [Child] = [S || S = #span{name = ~"child"} <- Spans],
    ?assertEqual(~"GET /child", Server#span.name),
    ?assertEqual(Server#span.trace_id, Child#span.trace_id),
    ?assertEqual(Server#span.span_id, Child#span.parent_span_id),
    Attrs = otel_attributes:map(Server#span.attributes),
    ?assertEqual(~"/child", maps:get('http.route', Attrs)),
    ?assertEqual(~"test_app", maps:get('nova.app', Attrs)).

sendfile_response_is_served_and_measured(Config) ->
    StaticBody = ?config(static_body, Config),
    {200, Body} = get("/static", []),
    ?assertEqual(StaticBody, Body),
    [Server] = [S || S = #span{kind = server} <- receive_spans(1)],
    Attrs = otel_attributes:map(Server#span.attributes),
    ?assertEqual(200, maps:get('http.response.status_code', Attrs)),
    Size = integer_to_binary(byte_size(StaticBody)),
    await_metric(
        <<"http_server_response_body_size_bytes_sum\\{[^}]*http_route=\"/static\"[^}]*\\} ",
            Size/binary>>
    ).

sendfile_response_logs_no_request_data(_Config) ->
    {200, _} = get("/static", [{"cookie", ?COOKIE}]),
    _ = receive_spans(1),
    assert_no_log_mentions("s3cr3t").

instrumentation_failure_logs_no_request_data(_Config) ->
    Req = #{
        method => ~"GET",
        headers => #{~"cookie" => list_to_binary(?COOKIE)},
        pid => self(),
        streamid => 1
    },
    {[], State0} = otel_nova_stream_h:init(1, Req, #{stream_handlers => []}),
    {[], State1} = otel_nova_stream_h:info(1, {response, 200, #{}, {not_a_body}}, State0),
    ok = otel_nova_stream_h:terminate(1, normal, State1),
    receive
        {log_event, #{msg := {report, #{what := otel_nova_instrumentation_failed}}}} -> ok
    after 1000 ->
        ct:fail(no_instrumentation_warning)
    end,
    assert_no_log_mentions("s3cr3t").

prometheus_endpoint_serves_recorded_metrics(_Config) ->
    {ok, _} = otel_nova_prom_server:start_link(#{port => ?METRICS_PORT}),
    try
        {200, _} = get("/child", []),
        Count = ~"http_server_request_duration_seconds_count\\{[^}]*http_route=\"/child\"",
        await_metric(Count),
        {200, Body} = request(?METRICS_PORT, "/metrics", []),
        ?assertMatch(
            {match, _}, re:run(Body, ~"# TYPE http_server_request_duration_seconds histogram")
        ),
        ?assertMatch({match, _}, re:run(Body, Count))
    after
        otel_nova_prom_server:stop()
    end.

active_requests_gauge_tracks_in_flight_requests(_Config) ->
    Req = #{
        method => ~"GET",
        path => ~"/active",
        scheme => ~"http",
        host => ~"active.test",
        port => 8080,
        version => 'HTTP/1.1',
        headers => #{},
        peer => {{127, 0, 0, 1}, 1},
        pid => self(),
        streamid => 1
    },
    {[], A} = otel_nova_stream_h:init(1, Req, #{stream_handlers => []}),
    otel_meter_server:force_flush(),
    {[], B} = otel_nova_stream_h:init(2, Req, #{stream_handlers => []}),
    otel_meter_server:force_flush(),
    ?assertEqual(~"2", active_requests(~"active.test")),
    otel_nova_stream_h:terminate(1, normal, A),
    otel_nova_stream_h:terminate(2, normal, B),
    otel_meter_server:force_flush(),
    ?assertEqual(~"0", active_requests(~"active.test")).

supervised_prom_server_stops_listener_with_app(_Config) ->
    load_test_app(supervised),
    {ok, _} = application:ensure_all_started(otel_nova_test_app),
    {200, _} = request(?METRICS_PORT, "/metrics", []),
    ok = application:stop(otel_nova_test_app),
    ?assertNot(maps:is_key(otel_nova_prom_listener, ranch:info())),
    ?assertMatch({error, _}, request(?METRICS_PORT, "/metrics", [])),
    {ok, _} = application:ensure_all_started(otel_nova_test_app),
    {200, _} = request(?METRICS_PORT, "/metrics", []).

setup_prom_server_survives_app_restart(_Config) ->
    load_test_app(setup),
    {ok, _} = application:ensure_all_started(otel_nova_test_app),
    {200, _} = request(?METRICS_PORT, "/metrics", []),
    ok = application:stop(otel_nova_test_app),
    {ok, _} = application:ensure_all_started(otel_nova_test_app),
    {200, _} = request(?METRICS_PORT, "/metrics", []).

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

load_test_app(Mode) ->
    ok = application:load(
        {application, otel_nova_test_app, [
            {description, "opentelemetry_nova test app"},
            {vsn, "0"},
            {modules, [otel_nova_test_app]},
            {registered, []},
            {applications, [kernel, stdlib, cowboy]},
            {mod, {otel_nova_test_app, []}},
            {env, [{mode, Mode}, {prometheus, #{port => ?METRICS_PORT}}]}
        ]}
    ).

get(Path, Headers) ->
    request(?HTTP_PORT, Path, Headers).

request(Port, Path, Headers) ->
    Url = "http://localhost:" ++ integer_to_list(Port) ++ Path,
    case httpc:request(get, {Url, Headers}, [], [{body_format, binary}]) of
        {ok, {{_, Status, _}, _RespHeaders, Body}} -> {Status, Body};
        {error, Reason} -> {error, Reason}
    end.

%% Cowboy may end the stream, and so the server span, after the client has
%% read the response, so both helpers flush until the data shows up.
receive_spans(N) ->
    receive_spans(N, [], 50).

receive_spans(0, Acc, _Attempts) ->
    Acc;
receive_spans(N, Acc, 0) ->
    ct:fail({timeout_waiting_for_spans, N, Acc});
receive_spans(N, Acc, Attempts) ->
    otel_tracer_provider:force_flush(),
    receive
        {span, Span} -> receive_spans(N - 1, [Span | Acc], Attempts)
    after 100 ->
        receive_spans(N, Acc, Attempts - 1)
    end.

await_metric(Regex) ->
    await_metric(Regex, 50).

await_metric(Regex, 0) ->
    ct:fail({metric_not_exported, Regex, otel_nova_prom_exporter:get_metrics()});
await_metric(Regex, Attempts) ->
    otel_meter_server:force_flush(),
    case re:run(otel_nova_prom_exporter:get_metrics(), Regex) of
        {match, _} ->
            ok;
        nomatch ->
            receive
            after 100 -> await_metric(Regex, Attempts - 1)
            end
    end.

active_requests(Host) ->
    Metrics = otel_nova_prom_exporter:get_metrics(),
    {match, [Value]} = re:run(
        Metrics,
        <<"http_server_active_requests\\{[^}]*server_address=\"", Host/binary,
            "\"[^}]*\\} (-?[0-9]+)">>,
        [{capture, [1], binary}]
    ),
    Value.

assert_no_log_mentions(Secret) ->
    receive
        {log_event, Event} ->
            ?assertEqual(nomatch, string:find(io_lib:format("~0tp", [Event]), Secret)),
            assert_no_log_mentions(Secret)
    after 500 ->
        ok
    end.
