-module(opentelemetry_nova_SUITE).
-behaviour(ct_suite).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include_lib("opentelemetry_api/include/otel_tracer.hrl").
-include_lib("opentelemetry_api/include/opentelemetry.hrl").
-include_lib("opentelemetry/include/otel_span.hrl").
-include_lib("opentelemetry_experimental/include/otel_metrics.hrl").

-define(assertReceiveMetric(Name, Fun),
    (fun() ->
        receive
            {otel_metric, M = #metric{name = N}} when N =:= Name ->
                (Fun)(M)
        after 5000 ->
            ct:fail({timeout_waiting_for_metric, Name})
        end
    end)()
).

%%--------------------------------------------------------------------
%% CT callbacks
%%--------------------------------------------------------------------

all() ->
    [
        stream_handler_creates_span,
        stream_handler_captures_status,
        stream_handler_sets_error_on_5xx,
        stream_handler_sets_error_type_on_5xx,
        stream_handler_ends_span_on_auth_rejection,
        stream_handler_protocol_version,
        stream_handler_client_address_xff,
        stream_handler_client_address_forwarded,
        stream_handler_client_address_peer_fallback,
        stream_handler_ignores_forwarded_for_by_default,
        stream_handler_rejects_invalid_forwarded_for,
        stream_handler_normalises_unknown_method,
        stream_handler_tags_stream_failure,
        stream_handler_sets_redacted_url_query,
        stream_handler_url_query_options,
        stream_handler_captures_request_headers,
        stream_handler_captures_response_headers,
        stream_handler_sets_opt_in_attributes,
        stream_handler_ignores_failure_after_response,
        plugin_enriches_span,
        plugin_sets_http_route,
        plugin_sets_http_route_with_bindings,
        plugin_folds_catch_all_tail_into_route,
        plugin_canonicalises_route,
        plugin_omits_undeterminable_route,
        plugin_prefers_route_from_nova,
        plugin_ignores_invalid_route_from_nova,
        plugin_sets_logger_metadata_with_trace_ids,
        plugin_tags_crash_from_crash_info,
        plugin_uses_span_name_formatter,
        plugin_falls_back_when_span_name_formatter_fails,
        plugin_ignores_crash_info_without_reason,
        plugin_skips_logger_metadata_without_valid_span,
        stream_handler_restores_context_on_terminate,
        trace_context_propagation,
        metrics_request_duration,
        metrics_request_duration_with_route,
        metrics_server_attributes_need_allowlist,
        metrics_error_type_in_metrics,
        metrics_active_requests,
        metrics_body_sizes
    ].

init_per_suite(Config) ->
    application:ensure_all_started(opentelemetry),
    ok = application:set_env(opentelemetry_experimental, readers, [
        #{
            module => otel_metric_reader,
            config => #{exporter => {test_metric_exporter, []}}
        }
    ]),
    application:ensure_all_started(opentelemetry_experimental),
    opentelemetry_nova:setup(),
    Config.

end_per_suite(_Config) ->
    application:stop(opentelemetry_experimental),
    application:stop(opentelemetry),
    ok.

init_per_testcase(_TestCase, Config) ->
    otel_batch_processor:set_exporter(otel_exporter_pid, self()),
    persistent_term:put(test_metric_pid, self()),
    Config.

end_per_testcase(_TestCase, _Config) ->
    persistent_term:erase(test_metric_pid),
    [
        application:unset_env(opentelemetry_nova, Key)
     || Key <- [
            trust_forwarded_for,
            metric_server_hosts,
            url_query,
            url_query_redact_keys,
            request_headers,
            response_headers,
            sensitive_headers,
            opt_in_attributes,
            span_name_formatter,
            trust_incoming_context
        ]
    ],
    otel_ctx:clear(),
    ok.

%%--------------------------------------------------------------------
%% Stream handler tests
%%--------------------------------------------------------------------

stream_handler_creates_span(_Config) ->
    Req = make_req(<<"GET">>, <<"/users">>),
    {_Cmds, State} = otel_nova_stream_h:init(1, Req, stream_opts()),
    otel_nova_stream_h:terminate(1, normal, State),

    receive
        {span, Span = #span{name = Name}} ->
            ?assertEqual(<<"HTTP GET">>, Name),
            Attrs = otel_nova_test_util:span_attrs(Span),
            ?assertEqual(<<"GET">>, maps:get('http.request.method', Attrs)),
            ?assertEqual(<<"/users">>, maps:get('url.path', Attrs)),
            ?assertEqual(<<"http">>, maps:get('url.scheme', Attrs)),
            ?assertEqual(<<"localhost">>, maps:get('server.address', Attrs)),
            ?assertEqual(8080, maps:get('server.port', Attrs)),
            ?assertMatch(<<"127.0.0.1">>, maps:get('network.peer.address', Attrs)),
            ?assertEqual(12345, maps:get('network.peer.port', Attrs)),
            ?assertEqual(<<"test-agent/1.0">>, maps:get('user_agent.original', Attrs)),
            ?assertEqual(<<"1.1">>, maps:get('network.protocol.version', Attrs)),
            ?assertEqual(<<"127.0.0.1">>, maps:get('client.address', Attrs))
    after 5000 ->
        ct:fail(timeout_waiting_for_span)
    end.

stream_handler_captures_status(_Config) ->
    Req = make_req(<<"GET">>, <<"/ok">>),
    {_Cmds, State0} = otel_nova_stream_h:init(1, Req, stream_opts()),
    {_Cmds1, State1} = otel_nova_stream_h:info(1, {response, 200, #{}, <<"OK">>}, State0),
    otel_nova_stream_h:terminate(1, normal, State1),

    receive
        {span, Span = #span{}} ->
            Attrs = otel_nova_test_util:span_attrs(Span),
            ?assertEqual(200, maps:get('http.response.status_code', Attrs))
    after 5000 ->
        ct:fail(timeout_waiting_for_span)
    end.

stream_handler_sets_error_on_5xx(_Config) ->
    Req = make_req(<<"POST">>, <<"/crash">>),
    {_Cmds, State0} = otel_nova_stream_h:init(1, Req, stream_opts()),
    {_Cmds1, State1} = otel_nova_stream_h:info(1, {response, 500, #{}, <<"Error">>}, State0),
    otel_nova_stream_h:terminate(1, normal, State1),

    receive
        {span, #span{status = Status}} ->
            ?assertMatch(#status{code = ?OTEL_STATUS_ERROR}, Status)
    after 5000 ->
        ct:fail(timeout_waiting_for_span)
    end.

stream_handler_sets_error_type_on_5xx(_Config) ->
    Req = make_req(<<"POST">>, <<"/crash">>),
    {_Cmds, State0} = otel_nova_stream_h:init(1, Req, stream_opts()),
    {_Cmds1, State1} = otel_nova_stream_h:info(1, {response, 503, #{}, <<"Unavailable">>}, State0),
    otel_nova_stream_h:terminate(1, normal, State1),

    receive
        {span, Span = #span{}} ->
            Attrs = otel_nova_test_util:span_attrs(Span),
            ?assertEqual(<<"503">>, maps:get('error.type', Attrs)),
            ?assertMatch(#status{code = ?OTEL_STATUS_ERROR}, Span#span.status)
    after 5000 ->
        ct:fail(timeout_waiting_for_span)
    end.

stream_handler_protocol_version(_Config) ->
    Req = make_req_with_version(<<"GET">>, <<"/test">>, 'HTTP/2'),
    {_Cmds, State} = otel_nova_stream_h:init(1, Req, stream_opts()),
    otel_nova_stream_h:terminate(1, normal, State),

    receive
        {span, Span = #span{}} ->
            Attrs = otel_nova_test_util:span_attrs(Span),
            ?assertEqual(<<"2">>, maps:get('network.protocol.version', Attrs))
    after 5000 ->
        ct:fail(timeout_waiting_for_span)
    end.

stream_handler_client_address_xff(_Config) ->
    ok = application:set_env(opentelemetry_nova, trust_forwarded_for, true),
    Req = make_req(<<"GET">>, <<"/test">>, #{<<"x-forwarded-for">> => <<"10.0.0.1, 10.0.0.2">>}),
    {_Cmds, State} = otel_nova_stream_h:init(1, Req, stream_opts()),
    otel_nova_stream_h:terminate(1, normal, State),

    receive
        {span, Span = #span{}} ->
            Attrs = otel_nova_test_util:span_attrs(Span),
            ?assertEqual(<<"10.0.0.1">>, maps:get('client.address', Attrs))
    after 5000 ->
        ct:fail(timeout_waiting_for_span)
    end.

stream_handler_client_address_forwarded(_Config) ->
    ok = application:set_env(opentelemetry_nova, trust_forwarded_for, true),
    Req = make_req(<<"GET">>, <<"/test">>, #{<<"forwarded">> => <<"for=192.168.1.1;proto=https">>}),
    {_Cmds, State} = otel_nova_stream_h:init(1, Req, stream_opts()),
    otel_nova_stream_h:terminate(1, normal, State),

    receive
        {span, Span = #span{}} ->
            Attrs = otel_nova_test_util:span_attrs(Span),
            ?assertEqual(<<"192.168.1.1">>, maps:get('client.address', Attrs))
    after 5000 ->
        ct:fail(timeout_waiting_for_span)
    end.

stream_handler_client_address_peer_fallback(_Config) ->
    Req = make_req(<<"GET">>, <<"/test">>),
    {_Cmds, State} = otel_nova_stream_h:init(1, Req, stream_opts()),
    otel_nova_stream_h:terminate(1, normal, State),

    receive
        {span, Span = #span{}} ->
            Attrs = otel_nova_test_util:span_attrs(Span),
            ?assertEqual(<<"127.0.0.1">>, maps:get('client.address', Attrs))
    after 5000 ->
        ct:fail(timeout_waiting_for_span)
    end.

stream_handler_ignores_forwarded_for_by_default(_Config) ->
    Req = make_req(~"GET", ~"/test", #{
        ~"x-forwarded-for" => ~"10.0.0.1", ~"forwarded" => ~"for=10.0.0.2"
    }),
    ?assertEqual(~"127.0.0.1", maps:get('client.address', span_attributes(stream_opts(), Req))).

stream_handler_rejects_invalid_forwarded_for(_Config) ->
    ok = application:set_env(opentelemetry_nova, trust_forwarded_for, true),
    [
        ?assertEqual(
            ~"127.0.0.1",
            maps:get(
                'client.address',
                span_attributes(stream_opts(), make_req(~"GET", ~"/", Headers))
            ),
            Headers
        )
     || Headers <- [
            #{~"x-forwarded-for" => <<16#FF>>},
            #{~"x-forwarded-for" => ~"not-an-ip, 10.0.0.1"},
            #{~"forwarded" => ~"for=\"_hidden\""}
        ]
    ],
    ?assertEqual(
        ~"2001:db8::1",
        maps:get(
            'client.address',
            span_attributes(
                stream_opts(),
                make_req(~"GET", ~"/", #{~"forwarded" => ~"for=\"[2001:db8::1]:4711\""})
            )
        )
    ).

stream_handler_normalises_unknown_method(_Config) ->
    Req = make_req(~"PROPFIND", ~"/users"),
    {State0, DownReq} = start_stream(1, Req),
    State = run_plugin(DownReq, #{}, State0),
    {_Cmds, State1} = otel_nova_stream_h:info(1, {response, 200, #{}, ~"OK"}, State),
    otel_nova_stream_h:terminate(1, normal, State1),
    otel_tracer_provider:force_flush(),
    receive
        {span, Span} ->
            Attrs = otel_nova_test_util:span_attrs(Span),
            ?assertEqual(~"HTTP /users", Span#span.name),
            ?assertEqual(~"_OTHER", maps:get('http.request.method', Attrs)),
            ?assertEqual(~"PROPFIND", maps:get('http.request.method_original', Attrs))
    after 5000 ->
        ct:fail(timeout_waiting_for_span)
    end,
    otel_meter_server:force_flush(),
    ?assertReceiveMetric('http.server.request.duration', fun(#metric{data = Data}) ->
        #histogram{datapoints = Datapoints} = Data,
        Methods = [
            Method
         || #histogram_datapoint{attributes = #{'http.request.method' := Method}} <- Datapoints
        ],
        ?assert(lists:member(~"_OTHER", Methods)),
        ?assertNot(lists:member(~"PROPFIND", Methods))
    end),
    GetReq = make_req(~"GET", ~"/users"),
    ?assertNot(maps:is_key('http.request.method_original', span_attributes(stream_opts(), GetReq))).

stream_handler_tags_stream_failure(_Config) ->
    [
        begin
            Path = atom_to_binary(element(1, Reason)),
            Req = make_req(~"GET", <<"/", Path/binary>>),
            {_Cmds, State} = otel_nova_stream_h:init(1, Req, stream_opts()),
            otel_nova_stream_h:terminate(1, Reason, State),
            otel_tracer_provider:force_flush(),
            Span = receive_span(<<"/", Path/binary>>),
            Attrs = otel_nova_test_util:span_attrs(Span),
            ?assertEqual(Path, maps:get('error.type', Attrs)),
            ?assertNot(maps:is_key('http.response.status_code', Attrs)),
            ?assertMatch(#status{code = ?OTEL_STATUS_ERROR, message = Path}, Span#span.status)
        end
     || Reason <- [
            {socket_error, closed, ~"The socket has been closed."},
            {internal_error, timeout, ~"Request timed out."},
            {stream_error, protocol_error, ~"Bad frame."},
            {connection_error, protocol_error, ~"Bad frame."},
            {stop, {exit, shutdown}, ~"Stopped."}
        ]
    ],
    otel_meter_server:force_flush(),
    ?assertReceiveMetric('http.server.request.duration', fun(#metric{data = Data}) ->
        #histogram{datapoints = Datapoints} = Data,
        [DP] = [
            DP
         || DP = #histogram_datapoint{attributes = #{'error.type' := ~"socket_error"}} <-
                Datapoints
        ],
        ?assertNot(maps:is_key('http.response.status_code', DP#histogram_datapoint.attributes)),
        ?assertEqual(1, DP#histogram_datapoint.count)
    end).

stream_handler_sets_redacted_url_query(_Config) ->
    Req = (make_req(~"GET", ~"/search"))#{
        qs =>
            ~"q=a+b&token=abc&Signature=x%2F&X-AMZ-Credential=y&b=%41&flag&p%61ssword=s&Code=1&key"
    },
    ?assertEqual(
        ~"q=a+b&token=REDACTED&Signature=REDACTED&X-AMZ-Credential=REDACTED&b=%41&flag&p%61ssword=REDACTED&Code=REDACTED&key",
        maps:get('url.query', span_attributes(stream_opts(), Req))
    ),
    ?assertNot(maps:is_key('url.query', span_attributes(stream_opts(), make_req(~"GET", ~"/")))),
    Invalid = (make_req(~"GET", ~"/"))#{qs => <<"q=", 16#FF>>},
    ?assertNot(maps:is_key('url.query', span_attributes(stream_opts(), Invalid))).

stream_handler_url_query_options(_Config) ->
    Req = (make_req(~"GET", ~"/search"))#{qs => ~"session_id=1&Tenant=a&token=t&q=x"},
    ok = application:set_env(opentelemetry_nova, url_query_redact_keys, [~"session_id", ~"TENANT"]),
    ?assertEqual(
        ~"session_id=REDACTED&Tenant=REDACTED&token=REDACTED&q=x",
        maps:get('url.query', span_attributes(stream_opts(), Req))
    ),
    ok = application:set_env(opentelemetry_nova, url_query, false),
    ?assertNot(maps:is_key('url.query', span_attributes(stream_opts(), Req))),
    ok = application:set_env(opentelemetry_nova, url_query, true),
    ok = application:set_env(opentelemetry_nova, url_query_redact_keys, not_a_list),
    Attrs = span_attributes(stream_opts(), Req),
    ?assertNot(maps:is_key('url.query', Attrs)),
    ?assertEqual(~"/search", maps:get('url.path', Attrs)).

stream_handler_captures_request_headers(_Config) ->
    ok = application:set_env(opentelemetry_nova, request_headers, [
        ~"x-request-id", ~"Authorization", ~"accept", ~"x-binary"
    ]),
    Req = make_req(~"GET", ~"/", #{
        ~"x-request-id" => ~"abc",
        ~"authorization" => ~"Bearer secret",
        ~"x-binary" => <<16#FF>>,
        ~"x-not-captured" => ~"1"
    }),
    Attrs = span_attributes(stream_opts(), Req),
    ?assertEqual(
        #{
            ~"http.request.header.x-request-id" => [~"abc"],
            ~"http.request.header.authorization" => [~"REDACTED"]
        },
        maps:filter(fun(K, _) -> is_binary(K) end, Attrs)
    ),
    ok = application:set_env(opentelemetry_nova, sensitive_headers, []),
    ?assertEqual(
        [~"Bearer secret"],
        maps:get(~"http.request.header.authorization", span_attributes(stream_opts(), Req))
    ).

stream_handler_captures_response_headers(_Config) ->
    ok = application:set_env(opentelemetry_nova, response_headers, [
        ~"content-type", ~"set-cookie", ~"x-trace"
    ]),
    Headers = #{
        ~"content-type" => ~"text/plain",
        ~"set-cookie" => [~"a=1", [~"b=", ~"2"]],
        ~"x-trace" => [~"t", ~"1"],
        ~"x-other" => ~"z"
    },
    ?assertEqual(
        #{
            ~"http.response.header.content-type" => [~"text/plain"],
            ~"http.response.header.set-cookie" => [~"REDACTED"],
            ~"http.response.header.x-trace" => [~"t1"]
        },
        response_header_attrs({response, 200, Headers, ~"ok"})
    ),
    ok = application:set_env(opentelemetry_nova, sensitive_headers, [~"content-type"]),
    ?assertEqual(
        #{
            ~"http.response.header.content-type" => [~"REDACTED"],
            ~"http.response.header.set-cookie" => [~"a=1", ~"b=2"],
            ~"http.response.header.x-trace" => [~"t1"]
        },
        response_header_attrs({headers, 200, Headers})
    ),
    ok = application:unset_env(opentelemetry_nova, response_headers),
    ?assertEqual(#{}, response_header_attrs({response, 200, Headers, ~"ok"})).

stream_handler_sets_opt_in_attributes(_Config) ->
    OptIn = [
        'client.port',
        'network.local.address',
        'network.local.port',
        'network.transport',
        'http.request.body.size',
        'http.response.body.size'
    ],
    Default = run_with_bodies(~"/opt-in-default"),
    [?assertNot(maps:is_key(Name, Default), Name) || Name <- OptIn],
    ok = application:set_env(opentelemetry_nova, opt_in_attributes, [unknown | OptIn]),
    Attrs = run_with_bodies(~"/opt-in"),
    ?assertEqual(
        #{
            'client.port' => 12345,
            'network.local.address' => ~"127.0.0.2",
            'network.local.port' => 8080,
            'network.transport' => ~"tcp",
            'http.request.body.size' => 5,
            'http.response.body.size' => 3
        },
        maps:with(OptIn, Attrs)
    ),
    ok = application:set_env(opentelemetry_nova, trust_forwarded_for, true),
    Forwarded = make_req(~"GET", ~"/", #{~"x-forwarded-for" => ~"10.0.0.1"}),
    ForwardedAttrs = span_attributes(stream_opts(), Forwarded),
    ?assertEqual(~"10.0.0.1", maps:get('client.address', ForwardedAttrs)),
    ?assertNot(maps:is_key('client.port', ForwardedAttrs)),
    ?assertEqual(
        ~"udp",
        maps:get(
            'network.transport',
            span_attributes(stream_opts(), make_req_with_version(~"GET", ~"/", 'HTTP/3'))
        )
    ).

run_with_bodies(Path) ->
    {_Cmds, State0} = otel_nova_stream_h:init(1, make_req(~"POST", Path), stream_opts()),
    {_Cmds1, State1} = otel_nova_stream_h:data(1, fin, ~"hello", State0),
    {_Cmds2, State2} = otel_nova_stream_h:info(1, {response, 200, #{}, ~"bye"}, State1),
    otel_nova_stream_h:terminate(1, normal, State2),
    otel_tracer_provider:force_flush(),
    otel_nova_test_util:span_attrs(receive_span(Path)).

stream_handler_ignores_failure_after_response(_Config) ->
    Req = make_req(~"GET", ~"/late-failure"),
    {_Cmds, State0} = otel_nova_stream_h:init(1, Req, stream_opts()),
    {_Cmds1, State1} = otel_nova_stream_h:info(1, {response, 200, #{}, ~"OK"}, State0),
    otel_nova_stream_h:terminate(1, {socket_error, closed, ~"closed"}, State1),
    otel_tracer_provider:force_flush(),
    Span = receive_span(~"/late-failure"),
    ?assertNot(maps:is_key('error.type', otel_nova_test_util:span_attrs(Span))),
    ?assertNotMatch(#status{code = ?OTEL_STATUS_ERROR}, Span#span.status),
    Normal = make_req(~"GET", ~"/no-response"),
    {_Cmds2, State2} = otel_nova_stream_h:init(1, Normal, stream_opts()),
    otel_nova_stream_h:terminate(1, normal, State2),
    otel_tracer_provider:force_flush(),
    ?assertNot(
        maps:is_key('error.type', otel_nova_test_util:span_attrs(receive_span(~"/no-response")))
    ).

stream_handler_ends_span_on_auth_rejection(_Config) ->
    %% Simulates what happens when nova_security_handler returns {stop, Req}:
    %% the stream handler's terminate/3 still runs and ends the span.
    Req = make_req(<<"GET">>, <<"/protected">>),
    {_Cmds, State0} = otel_nova_stream_h:init(1, Req, stream_opts()),
    {_Cmds1, State1} = otel_nova_stream_h:info(1, {response, 401, #{}, <<"Unauthorized">>}, State0),
    %% Cowboy calls terminate even when middleware stops early
    otel_nova_stream_h:terminate(1, normal, State1),

    receive
        {span, Span = #span{}} ->
            Attrs = otel_nova_test_util:span_attrs(Span),
            ?assertEqual(401, maps:get('http.response.status_code', Attrs)),
            %% 401 is not a server error, so no error status
            ?assertNotMatch(#status{code = ?OTEL_STATUS_ERROR}, Span#span.status)
    after 5000 ->
        ct:fail(timeout_waiting_for_span)
    end.

stream_handler_restores_context_on_terminate(_Config) ->
    %% Set up a pre-existing context value before the stream handler runs
    otel_ctx:set_value(test_key, test_value),

    Req = make_req(<<"GET">>, <<"/test">>),
    {_Cmds, State0} = otel_nova_stream_h:init(1, Req, stream_opts()),
    {_Cmds1, State1} = otel_nova_stream_h:info(1, {response, 200, #{}, <<"OK">>}, State0),
    otel_nova_stream_h:terminate(1, normal, State1),

    %% After terminate, the previous context should be restored (not cleared)
    ?assertEqual(test_value, otel_ctx:get_value(test_key, undefined)),

    receive
        {span, #span{}} -> ok
    after 5000 ->
        ct:fail(timeout_waiting_for_span)
    end,
    otel_ctx:clear().

%%--------------------------------------------------------------------
%% Plugin tests
%%--------------------------------------------------------------------

plugin_enriches_span(_Config) ->
    Req = make_req(<<"GET">>, <<"/users">>),
    {State0, DownReq} = start_stream(1, Req),

    Env = #{app => my_app, callback => fun fake_controller:index/1},
    State = run_plugin(DownReq, Env, State0),

    {_Cmds1, State1} = otel_nova_stream_h:info(1, {response, 200, #{}, <<"OK">>}, State),
    otel_nova_stream_h:terminate(1, normal, State1),

    receive
        {span, Span = #span{name = Name}} ->
            ?assertEqual(<<"GET /users">>, Name),
            Attrs = otel_nova_test_util:span_attrs(Span),
            ?assertEqual(<<"my_app">>, maps:get('nova.app', Attrs)),
            ?assertEqual(<<"fake_controller">>, maps:get('nova.controller', Attrs)),
            ?assertEqual(<<"index">>, maps:get('nova.action', Attrs)),
            ?assertEqual(<<"/users">>, maps:get('http.route', Attrs))
    after 5000 ->
        ct:fail(timeout_waiting_for_span)
    end.

plugin_sets_http_route(_Config) ->
    Req = make_req(<<"GET">>, <<"/api/health">>),
    {State0, DownReq} = start_stream(1, Req),

    Env = #{app => my_app, callback => fun fake_controller:health/1},
    State = run_plugin(DownReq, Env, State0),

    {_Cmds1, State1} = otel_nova_stream_h:info(1, {response, 200, #{}, <<"OK">>}, State),
    otel_nova_stream_h:terminate(1, normal, State1),

    receive
        {span, Span = #span{name = Name}} ->
            ?assertEqual(<<"GET /api/health">>, Name),
            Attrs = otel_nova_test_util:span_attrs(Span),
            ?assertEqual(<<"/api/health">>, maps:get('http.route', Attrs))
    after 5000 ->
        ct:fail(timeout_waiting_for_span)
    end.

plugin_sets_http_route_with_bindings(_Config) ->
    Req0 = make_req(<<"GET">>, <<"/users/42/posts/99">>),
    Req = Req0#{bindings => #{<<"user_id">> => <<"42">>, <<"post_id">> => <<"99">>}},
    {State0, DownReq} = start_stream(1, Req),

    Env = #{app => my_app, callback => fun fake_controller:show_post/1},
    State = run_plugin(DownReq, Env, State0),

    {_Cmds1, State1} = otel_nova_stream_h:info(1, {response, 200, #{}, <<"OK">>}, State),
    otel_nova_stream_h:terminate(1, normal, State1),

    receive
        {span, Span = #span{name = Name}} ->
            ?assertEqual(<<"GET /users/:user_id/posts/:post_id">>, Name),
            Attrs = otel_nova_test_util:span_attrs(Span),
            ?assertEqual(<<"/users/:user_id/posts/:post_id">>, maps:get('http.route', Attrs))
    after 5000 ->
        ct:fail(timeout_waiting_for_span)
    end.

plugin_folds_catch_all_tail_into_route(_Config) ->
    Req0 = make_req(~"GET", ~"/users/42/files/a/b.txt"),
    Req = Req0#{
        bindings => #{~"id" => ~"42"},
        extra_state => #{pathinfo => [~"a", ~"b.txt"]}
    },
    ?assertEqual(~"/users/:id/files/[...]", plugin_route(Req, #{})).

plugin_canonicalises_route(_Config) ->
    Req = make_req(~"GET", ~"//./api/x/../health/"),
    ?assertEqual(~"/api/health", plugin_route(Req, #{})).

plugin_omits_undeterminable_route(_Config) ->
    Req0 = make_req(~"GET", ~"/assets/app.css"),
    Req = Req0#{extra_state => #{pathinfo => [~"other.css"]}},
    ?assertEqual(undefined, plugin_route(Req, #{})).

plugin_prefers_route_from_nova(_Config) ->
    Req0 = make_req(~"GET", ~"/users/users"),
    Req = Req0#{bindings => #{~"id" => ~"users"}, nova_route => ~"/users/:id"},
    ?assertEqual(~"/users/:id", plugin_route(Req, #{})),
    Assets0 = make_req(~"GET", ~"/assets/a/b.css"),
    Assets = Assets0#{
        extra_state => #{pathinfo => [~"other.css"]}, nova_route => ~"/assets/[...]"
    },
    ?assertEqual(~"/assets/[...]", plugin_route(Assets, #{})).

plugin_ignores_invalid_route_from_nova(_Config) ->
    Req0 = make_req(~"GET", ~"/api/health"),
    [
        ?assertEqual(~"/api/health", plugin_route(Req0#{nova_route => Route}, #{}), Route)
     || Route <- [<<"/bad", 16#FF>>, '/api/:id', 42]
    ].

plugin_tags_crash_from_crash_info(_Config) ->
    [
        ?assertEqual({Tag, Tag, Tag}, crash_tags(CrashInfo, 500), CrashInfo)
     || {CrashInfo, Tag} <- [
            {#{class => error, reason => badarg}, ~"badarg"},
            {#{class => error, reason => {badmatch, 1}, stacktrace => []}, ~"badmatch"},
            {#{class => exit, reason => "text"}, ~"exit"},
            {#{class => "odd", reason => {}}, ~"_OTHER"}
        ]
    ],
    ?assertEqual(
        {~"badarg", ~"badarg", undefined}, crash_tags(#{class => error, reason => badarg}, 404)
    ).

plugin_uses_span_name_formatter(_Config) ->
    ok = application:set_env(
        opentelemetry_nova, span_name_formatter, {otel_nova_test_controller, span_name}
    ),
    ?assertEqual(~"custom GET /users", plugin_span_name(make_req(~"GET", ~"/users"))),
    ?assertEqual(~"custom _OTHER /users", plugin_span_name(make_req(~"PROPFIND", ~"/users"))),
    Unrouted = (make_req(~"GET", ~"/assets/app.css"))#{extra_state => #{pathinfo => [~"x.css"]}},
    ?assertEqual(~"GET unrouted", plugin_span_name(Unrouted)).

plugin_falls_back_when_span_name_formatter_fails(_Config) ->
    [
        begin
            ok = application:set_env(
                opentelemetry_nova, span_name_formatter, {otel_nova_test_controller, Fun}
            ),
            ?assertEqual(~"GET /users", plugin_span_name(make_req(~"GET", ~"/users")), Fun)
        end
     || Fun <- [crashing_span_name, invalid_span_name, not_exported]
    ].

plugin_ignores_crash_info_without_reason(_Config) ->
    [
        ?assertEqual({~"500", undefined, ~"500"}, crash_tags(CrashInfo, 500), CrashInfo)
     || CrashInfo <- [unavailable, #{status_code => 500, title => ~"Error in controller"}]
    ].

plugin_sets_logger_metadata_with_trace_ids(_Config) ->
    logger:unset_process_metadata(),
    Req = make_req(<<"GET">>, <<"/users">>),
    {State0, DownReq} = start_stream(1, Req),

    Env = #{app => my_app, callback => fun fake_controller:index/1},
    State = run_plugin(DownReq, Env, State0),

    Meta = logger:get_process_metadata(),
    ?assert(is_map(Meta)),
    TraceId = maps:get(trace_id, Meta, undefined),
    SpanId = maps:get(span_id, Meta, undefined),
    ?assert(is_binary(TraceId)),
    ?assert(is_binary(SpanId)),
    ?assertEqual(32, byte_size(TraceId)),
    ?assertEqual(16, byte_size(SpanId)),

    {_Cmds1, State1} = otel_nova_stream_h:info(1, {response, 200, #{}, <<"OK">>}, State),
    otel_nova_stream_h:terminate(1, normal, State1),
    drain_spans(),
    logger:unset_process_metadata().

plugin_skips_logger_metadata_without_valid_span(_Config) ->
    logger:unset_process_metadata(),
    otel_ctx:clear(),

    Req = make_req(<<"GET">>, <<"/no-span">>),
    Env = #{app => my_app, callback => fun fake_controller:index/1},
    {ok, _Req1, _PluginState} = otel_nova_plugin:pre_request(Req, Env, #{}, undefined),

    Meta =
        case logger:get_process_metadata() of
            undefined -> #{};
            M -> M
        end,
    ?assertNot(maps:is_key(trace_id, Meta)),
    ?assertNot(maps:is_key(span_id, Meta)),
    logger:unset_process_metadata().

%%--------------------------------------------------------------------
%% Trace context propagation
%%--------------------------------------------------------------------

trace_context_propagation(_Config) ->
    TraceId = otel_id_generator:generate_trace_id(),
    SpanId = otel_id_generator:generate_span_id(),
    TraceIdHex = list_to_binary(io_lib:format("~32.16.0b", [TraceId])),
    SpanIdHex = list_to_binary(io_lib:format("~16.16.0b", [SpanId])),
    Traceparent = <<"00-", TraceIdHex/binary, "-", SpanIdHex/binary, "-01">>,

    Req = make_req(<<"GET">>, <<"/traced">>, #{<<"traceparent">> => Traceparent}),
    {_Cmds, State} = otel_nova_stream_h:init(1, Req, stream_opts()),
    {_Cmds1, State1} = otel_nova_stream_h:info(1, {response, 200, #{}, <<"OK">>}, State),
    otel_nova_stream_h:terminate(1, normal, State1),

    receive
        {span, #span{trace_id = RecvTraceId, parent_span_id = ParentSpanId}} ->
            ?assertEqual(TraceId, RecvTraceId),
            ?assertEqual(SpanId, ParentSpanId)
    after 5000 ->
        ct:fail(timeout_waiting_for_span)
    end.

%%--------------------------------------------------------------------
%% Metric tests
%%--------------------------------------------------------------------

metrics_request_duration(_Config) ->
    Req = make_req(<<"GET">>, <<"/duration">>),
    {_Cmds, State0} = otel_nova_stream_h:init(1, Req, stream_opts()),
    {_Cmds1, State1} = otel_nova_stream_h:info(1, {response, 200, #{}, <<"OK">>}, State0),
    otel_nova_stream_h:terminate(1, normal, State1),
    drain_spans(),

    otel_meter_server:force_flush(),

    ?assertReceiveMetric('http.server.request.duration', fun(#metric{data = Data}) ->
        #histogram{datapoints = Datapoints} = Data,
        ?assertMatch([_ | _], Datapoints),
        DP = find_datapoint_with_status(200, Datapoints),
        ?assert(DP#histogram_datapoint.sum >= 0),
        ?assert(DP#histogram_datapoint.count >= 1),
        Attrs = DP#histogram_datapoint.attributes,
        ?assertEqual(<<"GET">>, maps:get('http.request.method', Attrs)),
        ?assertEqual(<<"http">>, maps:get('url.scheme', Attrs)),
        ?assertNotMatch(#{'server.address' := _}, Attrs),
        ?assertNotMatch(#{'server.port' := _}, Attrs)
    end).

metrics_server_attributes_need_allowlist(_Config) ->
    ok = application:set_env(opentelemetry_nova, metric_server_hosts, [~"allowed.test"]),
    Allowed = (make_req(~"GET", ~"/a"))#{
        host => ~"allowed.test", sock => {{127, 0, 0, 1}, 4000}
    },
    Other = (make_req(~"GET", ~"/a"))#{
        host => ~"other.test", sock => {{127, 0, 0, 1}, 4000}
    },
    [
        begin
            {_Cmds, State0} = otel_nova_stream_h:init(1, Req, stream_opts()),
            {_Cmds1, State1} = otel_nova_stream_h:info(1, {response, 202, #{}, ~"OK"}, State0),
            otel_nova_stream_h:terminate(1, normal, State1)
        end
     || Req <- [Allowed, Other]
    ],
    drain_spans(),
    otel_meter_server:force_flush(),
    ?assertReceiveMetric('http.server.request.duration', fun(#metric{data = Data}) ->
        #histogram{datapoints = Datapoints} = Data,
        Servers = lists:sort([
            {maps:get('server.address', A), maps:get('server.port', A)}
         || #histogram_datapoint{attributes = A = #{'http.response.status_code' := 202}} <-
                Datapoints
        ]),
        ?assertEqual([{~"_OTHER", 4000}, {~"allowed.test", 4000}], Servers)
    end).

metrics_request_duration_with_route(_Config) ->
    Req = make_req(<<"GET">>, <<"/users">>),
    {State, DownReq} = start_stream(1, Req),

    Env = #{app => my_app, callback => fun fake_controller:index/1},
    State0 = run_plugin(DownReq, Env, State),

    {_Cmds1, State1} = otel_nova_stream_h:info(1, {response, 200, #{}, <<"OK">>}, State0),
    otel_nova_stream_h:terminate(1, normal, State1),
    drain_spans(),

    otel_meter_server:force_flush(),

    ?assertReceiveMetric('http.server.request.duration', fun(#metric{data = Data}) ->
        #histogram{datapoints = Datapoints} = Data,
        DP = find_datapoint_with_status(200, Datapoints),
        Attrs = DP#histogram_datapoint.attributes,
        ?assertEqual(<<"/users">>, maps:get('http.route', Attrs)),
        ?assertEqual(200, maps:get('http.response.status_code', Attrs))
    end).

metrics_error_type_in_metrics(_Config) ->
    Req = make_req(<<"POST">>, <<"/crash">>),
    {_Cmds, State0} = otel_nova_stream_h:init(1, Req, stream_opts()),
    {_Cmds1, State1} = otel_nova_stream_h:info(1, {response, 500, #{}, <<"Error">>}, State0),
    otel_nova_stream_h:terminate(1, normal, State1),
    drain_spans(),

    otel_meter_server:force_flush(),

    ?assertReceiveMetric('http.server.request.duration', fun(#metric{data = Data}) ->
        #histogram{datapoints = Datapoints} = Data,
        DP = find_datapoint_with_status(500, Datapoints),
        Attrs = DP#histogram_datapoint.attributes,
        ?assertEqual(<<"500">>, maps:get('error.type', Attrs))
    end).

metrics_active_requests(_Config) ->
    Req = make_req(<<"GET">>, <<"/active">>),
    {_Cmds, State0} = otel_nova_stream_h:init(1, Req, stream_opts()),
    {_Cmds1, State1} = otel_nova_stream_h:info(1, {response, 200, #{}, <<"OK">>}, State0),
    otel_nova_stream_h:terminate(1, normal, State1),
    drain_spans(),

    otel_meter_server:force_flush(),

    %% After init (+1) and terminate (-1), net should be 0
    ?assertReceiveMetric('http.server.active_requests', fun(#metric{data = Data}) ->
        #sum{datapoints = Datapoints, is_monotonic = false} = Data,
        ?assertMatch([_ | _], Datapoints),
        [DP | _] = Datapoints,
        %% Net value after +1 and -1 should be 0
        ?assertEqual(0, DP#datapoint.value)
    end).

metrics_body_sizes(_Config) ->
    Req = make_req(<<"POST">>, <<"/upload">>),
    ReqBody = <<"hello world">>,
    RespBody = <<"response body here">>,

    {_Cmds, State0} = otel_nova_stream_h:init(1, Req, stream_opts()),
    {_Cmds1, State1} = otel_nova_stream_h:data(1, fin, ReqBody, State0),
    {_Cmds2, State2} = otel_nova_stream_h:info(1, {response, 200, #{}, RespBody}, State1),
    otel_nova_stream_h:terminate(1, normal, State2),
    drain_spans(),

    otel_meter_server:force_flush(),

    ?assertReceiveMetric('http.server.request.body.size', fun(#metric{data = Data}) ->
        #histogram{datapoints = Datapoints} = Data,
        [DP | _] = Datapoints,
        ?assertEqual(byte_size(ReqBody), DP#histogram_datapoint.sum),
        ?assert(DP#histogram_datapoint.count >= 1)
    end),

    ?assertReceiveMetric('http.server.response.body.size', fun(#metric{data = Data}) ->
        #histogram{datapoints = Datapoints} = Data,
        [DP | _] = Datapoints,
        ?assertEqual(byte_size(RespBody), DP#histogram_datapoint.sum),
        ?assert(DP#histogram_datapoint.count >= 1)
    end).

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

stream_opts() ->
    #{stream_handlers => [capture_stream_h]}.

start_stream(StreamID, Req) ->
    {_Cmds, State} = otel_nova_stream_h:init(StreamID, Req, stream_opts()),
    receive
        {downstream_req, DownReq} -> {State, DownReq}
    after 1000 ->
        ct:fail(no_downstream_req)
    end.

run_plugin(Req, Env, State) ->
    {ok, _Req, _PluginState} = otel_nova_plugin:pre_request(Req, Env, #{}, undefined),
    forward_casts(1, State).

span_attributes(Opts, Req) ->
    {_Cmds, State} = otel_nova_stream_h:init(1, Req, Opts),
    otel_nova_stream_h:terminate(1, normal, State),
    otel_tracer_provider:force_flush(),
    receive
        {span, Span} -> otel_nova_test_util:span_attrs(Span)
    after 5000 ->
        ct:fail(timeout_waiting_for_span)
    end.

plugin_span_name(Req) ->
    {State0, DownReq} = start_stream(1, Req),
    State = run_plugin(DownReq, #{}, State0),
    {_Cmds, State1} = otel_nova_stream_h:info(1, {response, 200, #{}, ~"OK"}, State),
    otel_nova_stream_h:terminate(1, normal, State1),
    otel_tracer_provider:force_flush(),
    #span{name = Name} = receive_span(maps:get(path, Req)),
    Name.

response_header_attrs(Command) ->
    Path = iolist_to_binary(["/headers/", integer_to_list(erlang:unique_integer([positive]))]),
    {_Cmds, State0} = otel_nova_stream_h:init(1, make_req(~"GET", Path), stream_opts()),
    {_Cmds1, State1} = otel_nova_stream_h:info(1, Command, State0),
    otel_nova_stream_h:terminate(1, normal, State1),
    otel_tracer_provider:force_flush(),
    Attrs = otel_nova_test_util:span_attrs(receive_span(Path)),
    maps:filter(fun(K, _) -> is_binary(K) end, Attrs).

plugin_route(Req, Env) ->
    {State0, DownReq} = start_stream(1, Req),
    State = run_plugin(DownReq, Env, State0),
    {_Cmds, State1} = otel_nova_stream_h:info(1, {response, 200, #{}, ~"OK"}, State),
    otel_nova_stream_h:terminate(1, normal, State1),
    otel_tracer_provider:force_flush(),
    receive
        {span, Span} ->
            {Route, Expected} =
                case otel_nova_test_util:span_attrs(Span) of
                    #{'http.route' := R} when is_binary(R) -> {R, <<"GET ", R/binary>>};
                    #{} -> {undefined, ~"HTTP GET"}
                end,
            ?assertEqual(Expected, Span#span.name),
            Route
    after 5000 ->
        ct:fail(timeout_waiting_for_span)
    end.

%% Runs a request whose post_request sees CrashInfo and returns the span
%% error.type, the span status message and the duration metric error.type.
crash_tags(CrashInfo, Status) ->
    Path = iolist_to_binary(["/crash/", integer_to_list(erlang:unique_integer([positive]))]),
    {State0, DownReq} = start_stream(1, make_req(~"GET", Path)),
    State1 = run_plugin(DownReq, #{}, State0),
    {ok, _, _} = otel_nova_plugin:post_request(
        DownReq#{crash_info => CrashInfo}, #{}, #{}, undefined
    ),
    State2 = forward_casts(1, State1),
    {_Cmds, State3} = otel_nova_stream_h:info(1, {response, Status, #{}, ~"Error"}, State2),
    otel_nova_stream_h:terminate(1, normal, State3),
    otel_tracer_provider:force_flush(),
    Span = receive_span(Path),
    SpanType = maps:get('error.type', otel_nova_test_util:span_attrs(Span), undefined),
    Message =
        case Span#span.status of
            #status{code = ?OTEL_STATUS_ERROR, message = M} when M =/= ~"Server error" -> M;
            _ -> undefined
        end,
    otel_meter_server:force_flush(),
    MetricType = metric_error_type(Path),
    {SpanType, Message, MetricType}.

receive_span(Path) ->
    receive
        {span, Span} ->
            case otel_nova_test_util:span_attrs(Span) of
                #{'url.path' := Path} -> Span;
                _ -> receive_span(Path)
            end
    after 5000 ->
        ct:fail({timeout_waiting_for_span, Path})
    end.

metric_error_type(Path) ->
    receive
        {otel_metric, #metric{name = 'http.server.request.duration', data = Data}} ->
            #histogram{datapoints = Datapoints} = Data,
            case
                [
                    maps:get('error.type', A, undefined)
                 || #histogram_datapoint{attributes = A = #{'http.route' := P}} <- Datapoints,
                    P =:= Path
                ]
            of
                [Type] -> Type;
                [] -> metric_error_type(Path)
            end;
        {otel_metric, _} ->
            metric_error_type(Path)
    after 5000 ->
        ct:fail({timeout_waiting_for_metric, Path})
    end.

forward_casts(StreamID, State0) ->
    receive
        {{_Pid, StreamID}, Msg = {_Tag, _Value}} ->
            {[], State} = otel_nova_stream_h:info(StreamID, Msg, State0),
            forward_casts(StreamID, State)
    after 0 ->
        State0
    end.

make_req(Method, Path) ->
    make_req(Method, Path, #{}).

make_req(Method, Path, ExtraHeaders) ->
    BaseHeaders = #{
        <<"host">> => <<"localhost:8080">>,
        <<"user-agent">> => <<"test-agent/1.0">>
    },
    Headers = maps:merge(BaseHeaders, ExtraHeaders),
    #{
        method => Method,
        path => Path,
        scheme => <<"http">>,
        host => <<"localhost">>,
        port => 8080,
        qs => <<>>,
        version => 'HTTP/1.1',
        headers => Headers,
        peer => {{127, 0, 0, 1}, 12345},
        sock => {{127, 0, 0, 2}, 8080},
        cert => undefined,
        pid => self(),
        streamid => 1
    }.

make_req_with_version(Method, Path, Version) ->
    Req = make_req(Method, Path),
    Req#{version => Version}.

drain_spans() ->
    receive
        {span, _} -> drain_spans()
    after 100 -> ok
    end.

find_datapoint_with_status(Code, [DP = #histogram_datapoint{attributes = Attrs} | Rest]) ->
    case maps:get('http.response.status_code', Attrs, undefined) of
        Code -> DP;
        _ -> find_datapoint_with_status(Code, Rest)
    end;
find_datapoint_with_status(Code, []) ->
    ct:fail({no_datapoint_with_status, Code}).
