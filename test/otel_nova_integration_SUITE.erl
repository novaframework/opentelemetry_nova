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
    active_requests_return_to_zero_when_metric_recording_fails/1,
    supervised_prom_server_stops_listener_with_app/1,
    setup_prom_server_survives_app_restart/1,
    catch_all_paths_share_one_route_series/1,
    plugin_failure_does_not_fail_the_request/1,
    request_labels_are_bounded/1,
    invalid_utf8_from_h2_keeps_exposition_valid/1,
    invalid_forwarded_for_still_records_request/1,
    prom_server_binds_loopback_by_default/1,
    controller_crash_sets_error_type/1,
    controller_crash_with_tagged_tuple_sets_error_type/1,
    thrown_status_is_not_tagged/1,
    plugin_crash_sets_error_type/1,
    security_crash_keeps_status_error_type/1,
    response_headers_are_captured/1,
    controller_adds_metric_attributes/1
]).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include_lib("opentelemetry_api/include/opentelemetry.hrl").
-include_lib("opentelemetry/include/otel_span.hrl").

-define(HTTP_PORT, 58607).
-define(METRICS_PORT, 58617).
-define(NOVA_PORT, 58627).
-define(LISTENER, otel_nova_integration_http).
-define(NOVA_LISTENER, otel_nova_integration_nova).
-define(COOKIE, "session=s3cr3t-cookie-value").
-define(SAMPLE_LINE,
    ~"^[a-zA-Z_:][a-zA-Z0-9_:]*(\\{[a-zA-Z_][a-zA-Z0-9_]*=\"(?:[^\"\\\\\\n]|\\\\[\\\\\"n])*\"(,[a-zA-Z_][a-zA-Z0-9_]*=\"(?:[^\"\\\\\\n]|\\\\[\\\\\"n])*\")*\\})? [-+0-9.eE]+(Inf)?$"
).

all() ->
    [
        controller_span_is_child_of_server_span,
        sendfile_response_is_served_and_measured,
        sendfile_response_logs_no_request_data,
        instrumentation_failure_logs_no_request_data,
        prometheus_endpoint_serves_recorded_metrics,
        active_requests_gauge_tracks_in_flight_requests,
        active_requests_return_to_zero_when_metric_recording_fails,
        supervised_prom_server_stops_listener_with_app,
        setup_prom_server_survives_app_restart,
        catch_all_paths_share_one_route_series,
        plugin_failure_does_not_fail_the_request,
        request_labels_are_bounded,
        invalid_utf8_from_h2_keeps_exposition_valid,
        invalid_forwarded_for_still_records_request,
        prom_server_binds_loopback_by_default,
        controller_crash_sets_error_type,
        controller_crash_with_tagged_tuple_sets_error_type,
        thrown_status_is_not_tagged,
        plugin_crash_sets_error_type,
        security_crash_keeps_status_error_type,
        response_headers_are_captured,
        controller_adds_metric_attributes
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
    {ok, Handlers} = nova_handlers:start_link(),
    unlink(Handlers),
    persistent_term:put(nova_dispatch, nova_routing_trie:new()),
    {ok, _} = cowboy:start_clear(?NOVA_LISTENER, [{port, ?NOVA_PORT}], #{
        stream_handlers => [otel_nova_stream_h, cowboy_stream_h],
        middlewares => [
            otel_nova_test_router,
            nova_plugin_handler,
            nova_security_handler,
            nova_handler,
            nova_plugin_handler
        ]
    }),
    [
        {plugin_manager, PluginManager},
        {nova_handlers, Handlers},
        {static_body, StaticBody}
        | Config
    ].

end_per_suite(Config) ->
    ok = cowboy:stop_listener(?LISTENER),
    ok = cowboy:stop_listener(?NOVA_LISTENER),
    persistent_term:erase(nova_dispatch),
    gen_server:stop(?config(nova_handlers, Config)),
    gen_server:stop(?config(plugin_manager, Config)),
    application:stop(opentelemetry_experimental),
    application:stop(opentelemetry),
    ok.

init_per_testcase(TestCase, Config) when
    TestCase =:= sendfile_response_logs_no_request_data;
    TestCase =:= instrumentation_failure_logs_no_request_data;
    TestCase =:= plugin_failure_does_not_fail_the_request
->
    ok = logger:add_handler(?MODULE, ?MODULE, #{config => #{pid => self()}}),
    init_per_testcase(default, Config);
init_per_testcase(_TestCase, Config) ->
    otel_batch_processor:set_exporter(otel_exporter_pid, self()),
    Config.

end_per_testcase(_TestCase, _Config) ->
    _ = logger:remove_handler(?MODULE),
    _ = application:unset_env(opentelemetry_nova, metric_server_hosts),
    _ = application:unset_env(opentelemetry_nova, trust_forwarded_for),
    _ = application:unset_env(opentelemetry_nova, response_headers),
    _ = application:unset_env(opentelemetry_nova, request_headers),
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
    Attrs = otel_nova_test_util:span_attrs(Server),
    ?assertEqual(~"/child", maps:get('http.route', Attrs)),
    ?assertEqual(~"test_app", maps:get('nova.app', Attrs)).

sendfile_response_is_served_and_measured(Config) ->
    StaticBody = ?config(static_body, Config),
    {200, Body} = get("/static", []),
    ?assertEqual(StaticBody, Body),
    [Server] = [S || S = #span{kind = server} <- receive_spans(1)],
    Attrs = otel_nova_test_util:span_attrs(Server),
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
    Req0 = otel_nova_test_util:stream_req(~"GET", ~"/"),
    Req = Req0#{headers => #{~"cookie" => list_to_binary(?COOKIE)}},
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
    ok = application:set_env(opentelemetry_nova, metric_server_hosts, [~"active.test"]),
    Req0 = otel_nova_test_util:stream_req(~"GET", ~"/active"),
    Req = Req0#{host => ~"active.test"},
    {[], A} = otel_nova_stream_h:init(1, Req, #{stream_handlers => []}),
    otel_meter_server:force_flush(),
    {[], B} = otel_nova_stream_h:init(2, Req, #{stream_handlers => []}),
    otel_meter_server:force_flush(),
    ?assertEqual(~"2", active_requests(~"active.test")),
    otel_nova_stream_h:terminate(1, normal, A),
    otel_nova_stream_h:terminate(2, normal, B),
    otel_meter_server:force_flush(),
    ?assertEqual(~"0", active_requests(~"active.test")).

active_requests_return_to_zero_when_metric_recording_fails(_Config) ->
    ok = application:set_env(opentelemetry_nova, metric_server_hosts, [~"leak.test"]),
    Req0 = otel_nova_test_util:stream_req(~"GET", ~"/leak"),
    {[], State} = otel_nova_stream_h:init(1, Req0#{host => ~"leak.test"}, #{stream_handlers => []}),
    otel_meter_server:force_flush(),
    ?assertEqual(~"1", active_requests(~"leak.test")),
    otel_nova_stream_h:terminate(1, normal, without_start_time(State)),
    otel_meter_server:force_flush(),
    ?assertEqual(~"0", active_requests(~"leak.test")).

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

catch_all_paths_share_one_route_series(_Config) ->
    [
        {404, _} = get("/assets/missing-" ++ integer_to_list(N) ++ ".css", [])
     || N <- lists:seq(1, 100)
    ],
    _ = receive_spans(100),
    await_metric(
        ~"http_server_request_duration_seconds_count\\{[^}]*http_route=\"/assets/\\[\\.\\.\\.\\]\"[^}]*\\} 100"
    ),
    Series = re:run(
        otel_nova_prom_exporter:get_metrics(),
        ~"http_server_request_duration_seconds_count\\{[^}]*http_route=\"/assets",
        [global]
    ),
    ?assertMatch({match, [_]}, Series).

plugin_failure_does_not_fail_the_request(_Config) ->
    {200, ~"ok"} = get("/plugin_crash", [{"cookie", ?COOKIE}]),
    _ = receive_spans(1),
    receive
        {log_event, #{
            msg := {report, #{what := otel_nova_instrumentation_failed, callback := pre_request}}
        }} ->
            ok
    after 1000 ->
        ct:fail(no_instrumentation_warning)
    end,
    assert_no_log_mentions("s3cr3t").

request_labels_are_bounded(_Config) ->
    [
        {ok, _} = raw(
            [
                ["M", integer_to_list(N), " /child HTTP/1.1\r\n"],
                ["host: h", integer_to_list(N), ".example:", integer_to_list(1000 + N), "\r\n"],
                "connection: close\r\n\r\n"
            ]
        )
     || N <- lists:seq(1, 300)
    ],
    _ = receive_spans(600),
    await_metric(
        ~"http_server_request_duration_seconds_count\\{http_request_method=\"_OTHER\",[^}]*\\} 300"
    ),
    Metrics = otel_nova_prom_exporter:get_metrics(),
    ?assertEqual(nomatch, binary:match(Metrics, ~".example")),
    ?assertEqual(nomatch, binary:match(Metrics, ~"M17")),
    ?assertMatch(
        {match, [_]},
        re:run(Metrics, ~"http_server_request_duration_seconds_count\\{[^}]*_OTHER", [global])
    ),
    ?assert(length(ets:match(otel_nova_prom_metrics, {{dp, '_', '_'}, '_'})) < 50).

invalid_utf8_from_h2_keeps_exposition_valid(_Config) ->
    {ok, Socket} = gen_tcp:connect("127.0.0.1", ?HTTP_PORT, [binary, {active, false}]),
    {Headers, _} = cow_hpack:encode([
        {~":method", <<"G\"E\nT}", 16#FF>>},
        {~":scheme", <<"ht\\tp\"x", 16#FE>>},
        {~":authority", ~"h2.example"},
        {~":path", ~"/child"}
    ]),
    ok = gen_tcp:send(Socket, [
        ~"PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n",
        cow_http2:settings(#{}),
        cow_http2:headers(1, fin, Headers)
    ]),
    {ok, _} = gen_tcp:recv(Socket, 0, 2000),
    _ = receive_spans(2),
    ok = gen_tcp:close(Socket),
    await_metric(~"url_scheme=\"_OTHER\""),
    Metrics = otel_nova_prom_exporter:get_metrics(),
    ?assertEqual(Metrics, unicode:characters_to_binary(Metrics)),
    {200, Scraped} = scrape(),
    [
        ?assertMatch({match, _}, re:run(Line, ?SAMPLE_LINE), Line)
     || Line <- binary:split(Scraped, ~"\n", [global, trim_all]),
        binary:first(Line) =/= $#
    ].

invalid_forwarded_for_still_records_request(_Config) ->
    Request = [
        "GET /child HTTP/1.1\r\nhost: xff.test\r\n",
        <<"x-forwarded-for: ", 16#FF, "\r\n">>,
        "connection: close\r\n\r\n"
    ],
    ok = application:set_env(opentelemetry_nova, metric_server_hosts, [~"xff.test"]),
    {ok, _} = raw(Request),
    ok = application:set_env(opentelemetry_nova, trust_forwarded_for, true),
    {ok, _} = raw(Request),
    Spans = [S || S = #span{kind = server} <- receive_spans(4)],
    ?assertEqual(2, length(Spans)),
    [
        ?assertEqual(~"127.0.0.1", maps:get('client.address', otel_nova_test_util:span_attrs(S)))
     || S <- Spans
    ],
    await_metric(
        ~"http_server_request_duration_seconds_count\\{[^}]*server_address=\"xff.test\"[^}]*\\} 2"
    ).

prom_server_binds_loopback_by_default(_Config) ->
    {ok, _} = otel_nova_prom_server:start_link(#{port => ?METRICS_PORT}),
    try
        ?assertEqual({{127, 0, 0, 1}, ?METRICS_PORT}, ranch:get_addr(otel_nova_prom_listener))
    after
        otel_nova_prom_server:stop()
    end,
    {ok, _} = otel_nova_prom_server:start_link(#{port => ?METRICS_PORT, ip => {0, 0, 0, 0}}),
    try
        ?assertEqual({{0, 0, 0, 0}, ?METRICS_PORT}, ranch:get_addr(otel_nova_prom_listener))
    after
        otel_nova_prom_server:stop()
    end.

controller_crash_sets_error_type(_Config) ->
    assert_crash_tagged("/nova/crash", ~"badarg").

controller_crash_with_tagged_tuple_sets_error_type(_Config) ->
    assert_crash_tagged("/nova/crash_tuple", ~"badmatch").

%% A known Nova bug: a pre_request plugin crash skips the post_request plugins
%% and runs the pre_request plugins a second time, where otel_nova_plugin
%% picks up crash_info.
plugin_crash_sets_error_type(_Config) ->
    assert_crash_tagged("/nova/plugin_crash", ~"plugin_boom").

thrown_status_is_not_tagged(_Config) ->
    {503, _} = request(?NOVA_PORT, "/nova/throw_status", []),
    [Server] = [S || S = #span{kind = server} <- receive_spans(1)],
    ?assertEqual(~"503", maps:get('error.type', otel_nova_test_util:span_attrs(Server))).

%% A crashing security function stops the middleware chain before any plugin
%% sees crash_info.
security_crash_keeps_status_error_type(_Config) ->
    {500, _} = request(?NOVA_PORT, "/nova/security_crash", []),
    [Server] = [S || S = #span{kind = server} <- receive_spans(1)],
    ?assertEqual(~"500", maps:get('error.type', otel_nova_test_util:span_attrs(Server))).

response_headers_are_captured(_Config) ->
    ok = application:set_env(opentelemetry_nova, request_headers, [~"cookie", ~"x-request-id"]),
    ok = application:set_env(opentelemetry_nova, response_headers, [
        ~"content-length", ~"server", ~"transfer-encoding"
    ]),
    {200, _} = get("/child", [{"cookie", ?COOKIE}, {"x-request-id", "r-1"}]),
    [Server] = [S || S = #span{kind = server} <- receive_spans(2)],
    Attrs = otel_nova_test_util:span_attrs(Server),
    ?assertEqual([~"REDACTED"], maps:get(~"http.request.header.cookie", Attrs)),
    ?assertEqual([~"r-1"], maps:get(~"http.request.header.x-request-id", Attrs)),
    ?assertEqual([~"2"], maps:get(~"http.response.header.content-length", Attrs)),
    ?assertEqual([~"Cowboy"], maps:get(~"http.response.header.server", Attrs)),
    ?assertNot(maps:is_key(~"http.response.header.transfer-encoding", Attrs)).

controller_adds_metric_attributes(_Config) ->
    {200, _} = request(?NOVA_PORT, "/nova/labelled", []),
    _ = receive_spans(1),
    await_metric(
        ~"http_server_request_duration_seconds_count\\{[^}]*http_route=\"/nova/labelled\"[^}]*tenant=\"acme\"[^}]*\\} 1"
    ),
    Metrics = otel_nova_prom_exporter:get_metrics(),
    ?assertEqual(nomatch, binary:match(Metrics, ~"/overridden")),
    ?assertNotMatch({match, _}, re:run(Metrics, ~"http_server_active_requests\\{[^}]*tenant=")).

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

assert_crash_tagged(Path, Tag) ->
    {500, _} = request(?NOVA_PORT, Path, []),
    [Server] = [S || S = #span{kind = server} <- receive_spans(1)],
    Attrs = otel_nova_test_util:span_attrs(Server),
    ?assertEqual(Tag, maps:get('error.type', Attrs)),
    ?assertEqual(500, maps:get('http.response.status_code', Attrs)),
    ?assertMatch(#status{code = ?OTEL_STATUS_ERROR, message = Tag}, Server#span.status),
    await_metric(
        <<"http_server_request_duration_seconds_count\\{[^}]*error_type=\"", Tag/binary,
            "\"[^}]*http_route=\"", (list_to_binary(Path))/binary, "\"">>
    ).

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

%% Clearing req_start makes the duration calculation in record_metrics raise.
-spec without_start_time(dynamic()) -> dynamic().
without_start_time(State) ->
    ReqStartField = 6,
    setelement(ReqStartField, State, undefined).

raw(Request) ->
    {ok, Socket} = gen_tcp:connect("127.0.0.1", ?HTTP_PORT, [binary, {active, false}]),
    ok = gen_tcp:send(Socket, Request),
    Result = gen_tcp:recv(Socket, 0, 2000),
    ok = gen_tcp:close(Socket),
    Result.

scrape() ->
    {ok, _} = otel_nova_prom_server:start_link(#{port => ?METRICS_PORT}),
    try
        request(?METRICS_PORT, "/metrics", [])
    after
        otel_nova_prom_server:stop()
    end.

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
