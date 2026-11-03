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
        plugin_enriches_span,
        plugin_sets_http_route,
        plugin_sets_http_route_with_bindings,
        plugin_folds_catch_all_tail_into_route,
        plugin_canonicalises_route,
        plugin_omits_undeterminable_route,
        plugin_sets_logger_metadata_with_trace_ids,
        plugin_skips_logger_metadata_without_valid_span,
        stream_handler_restores_context_on_terminate,
        trace_context_propagation,
        metrics_request_duration,
        metrics_request_duration_with_route,
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
            Attrs = otel_attributes:map(Span#span.attributes),
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
            Attrs = otel_attributes:map(Span#span.attributes),
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
            Attrs = otel_attributes:map(Span#span.attributes),
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
            Attrs = otel_attributes:map(Span#span.attributes),
            ?assertEqual(<<"2">>, maps:get('network.protocol.version', Attrs))
    after 5000 ->
        ct:fail(timeout_waiting_for_span)
    end.

stream_handler_client_address_xff(_Config) ->
    Req = make_req(<<"GET">>, <<"/test">>, #{<<"x-forwarded-for">> => <<"10.0.0.1, 10.0.0.2">>}),
    {_Cmds, State} = otel_nova_stream_h:init(1, Req, stream_opts()),
    otel_nova_stream_h:terminate(1, normal, State),

    receive
        {span, Span = #span{}} ->
            Attrs = otel_attributes:map(Span#span.attributes),
            ?assertEqual(<<"10.0.0.1">>, maps:get('client.address', Attrs))
    after 5000 ->
        ct:fail(timeout_waiting_for_span)
    end.

stream_handler_client_address_forwarded(_Config) ->
    Req = make_req(<<"GET">>, <<"/test">>, #{<<"forwarded">> => <<"for=192.168.1.1;proto=https">>}),
    {_Cmds, State} = otel_nova_stream_h:init(1, Req, stream_opts()),
    otel_nova_stream_h:terminate(1, normal, State),

    receive
        {span, Span = #span{}} ->
            Attrs = otel_attributes:map(Span#span.attributes),
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
            Attrs = otel_attributes:map(Span#span.attributes),
            ?assertEqual(<<"127.0.0.1">>, maps:get('client.address', Attrs))
    after 5000 ->
        ct:fail(timeout_waiting_for_span)
    end.

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
            Attrs = otel_attributes:map(Span#span.attributes),
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
            Attrs = otel_attributes:map(Span#span.attributes),
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
            Attrs = otel_attributes:map(Span#span.attributes),
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
            Attrs = otel_attributes:map(Span#span.attributes),
            ?assertEqual(<<"/users/:user_id/posts/:post_id">>, maps:get('http.route', Attrs))
    after 5000 ->
        ct:fail(timeout_waiting_for_span)
    end.

plugin_folds_catch_all_tail_into_route(_Config) ->
    Req0 = make_req(<<"GET">>, <<"/users/42/files/a/b.txt">>),
    Req = Req0#{
        bindings => #{<<"id">> => <<"42">>},
        extra_state => #{pathinfo => [<<"a">>, <<"b.txt">>]}
    },
    ?assertEqual(<<"/users/:id/files/[...]">>, plugin_route(Req, #{})).

plugin_canonicalises_route(_Config) ->
    Req = make_req(<<"GET">>, <<"//./api/x/../health/">>),
    ?assertEqual(<<"/api/health">>, plugin_route(Req, #{})).

plugin_omits_undeterminable_route(_Config) ->
    Req0 = make_req(<<"GET">>, <<"/assets/app.css">>),
    Req = Req0#{extra_state => #{pathinfo => [<<"other.css">>]}},
    ?assertEqual(undefined, plugin_route(Req, #{})).

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
        ?assertEqual(<<"localhost">>, maps:get('server.address', Attrs)),
        ?assertEqual(8080, maps:get('server.port', Attrs))
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

plugin_route(Req, Env) ->
    {State0, DownReq} = start_stream(1, Req),
    State = run_plugin(DownReq, Env, State0),
    {_Cmds, State1} = otel_nova_stream_h:info(1, {response, 200, #{}, <<"OK">>}, State),
    otel_nova_stream_h:terminate(1, normal, State1),
    receive
        {span, Span} ->
            Attrs = otel_attributes:map(Span#span.attributes),
            Route = maps:get('http.route', Attrs, undefined),
            Expected =
                case Route of
                    undefined -> <<"HTTP GET">>;
                    _ -> <<"GET ", Route/binary>>
                end,
            ?assertEqual(Expected, Span#span.name),
            Route
    after 5000 ->
        ct:fail(timeout_waiting_for_span)
    end.

forward_casts(StreamID, State0) ->
    receive
        {{_Pid, StreamID}, Msg = {otel_nova_route, _}} ->
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
