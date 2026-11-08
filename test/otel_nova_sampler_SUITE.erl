-module(otel_nova_sampler_SUITE).
-behaviour(ct_suite).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include_lib("opentelemetry_api/include/otel_tracer.hrl").
-include_lib("opentelemetry_api/include/opentelemetry.hrl").
-include_lib("opentelemetry/include/otel_span.hrl").
-include_lib("opentelemetry/include/otel_sampler.hrl").

-define(TRACE_ID, 16#0af7651916cd43dd8448eb211c80319c).
-define(SPAN_ID, 16#b7ad6b7169203331).
-define(TRACEPARENT, ~"00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01").

all() ->
    [
        excludes_matching_server_span,
        records_non_matching_server_span,
        records_non_server_span_even_if_name_matches,
        stream_handler_excluded_request_is_not_sampled,
        stream_handler_other_request_is_sampled,
        sampled_traceparent_forces_excluded_request_by_default,
        untrusted_context_does_not_force_sampling,
        untrusted_context_starts_new_trace_with_link,
        map_config_delegates_to_sampler,
        map_config_defaults_to_always_on,
        description_includes_delegate,
        trust_callback_decides_per_request,
        crashing_trust_callback_does_not_trust
    ].

init_per_suite(Config) ->
    application:load(opentelemetry),
    Sampler =
        {parent_based, #{root => {otel_nova_sampler, [<<"GET /metrics">>, <<"GET /healthz">>]}}},
    Processor = {otel_simple_processor, #{exporter => {otel_exporter_pid, self()}}},
    application:set_env(opentelemetry, sampler, Sampler),
    application:set_env(opentelemetry, processors, [Processor]),
    {ok, _} = application:ensure_all_started(opentelemetry),
    Config.

end_per_suite(_Config) ->
    application:stop(opentelemetry),
    application:unload(opentelemetry),
    ok.

init_per_testcase(_TestCase, Config) ->
    otel_simple_processor:set_exporter(otel_exporter_pid, self()),
    Config.

end_per_testcase(_TestCase, _Config) ->
    _ = application:unset_env(opentelemetry_nova, trust_incoming_context),
    otel_ctx:clear(),
    ok.

excludes_matching_server_span(_Config) ->
    ?with_span(<<"GET /metrics">>, #{kind => ?SPAN_KIND_SERVER}, fun(_) -> ok end),
    ?with_span(<<"GET /healthz">>, #{kind => ?SPAN_KIND_SERVER}, fun(_) -> ok end),
    receive
        {span, #span{}} -> ct:fail(should_not_receive_excluded_span)
    after 200 ->
        ok
    end.

records_non_matching_server_span(_Config) ->
    ?with_span(<<"GET /users">>, #{kind => ?SPAN_KIND_SERVER}, fun(_) -> ok end),
    receive
        {span, #span{name = Name}} ->
            ?assertEqual(<<"GET /users">>, Name)
    after 5000 ->
        ct:fail(timeout_waiting_for_span)
    end.

records_non_server_span_even_if_name_matches(_Config) ->
    ?with_span(<<"GET /metrics">>, #{kind => ?SPAN_KIND_CLIENT}, fun(_) -> ok end),
    receive
        {span, #span{name = Name}} ->
            ?assertEqual(<<"GET /metrics">>, Name)
    after 5000 ->
        ct:fail(timeout_waiting_for_span)
    end.

stream_handler_excluded_request_is_not_sampled(_Config) ->
    run_request(~"GET", ~"/metrics"),
    receive
        {span, #span{} = Span} -> ct:fail({should_not_sample, Span})
    after 200 ->
        ok
    end.

stream_handler_other_request_is_sampled(_Config) ->
    run_request(~"GET", ~"/users"),
    receive
        {span, #span{name = Name}} ->
            ?assertEqual(~"HTTP GET", Name)
    after 5000 ->
        ct:fail(timeout_waiting_for_span)
    end.

sampled_traceparent_forces_excluded_request_by_default(_Config) ->
    run_request(~"GET", ~"/metrics", #{~"traceparent" => ?TRACEPARENT}),
    receive
        {span, #span{trace_id = TraceId}} -> ?assertEqual(?TRACE_ID, TraceId)
    after 5000 ->
        ct:fail(timeout_waiting_for_span)
    end.

untrusted_context_does_not_force_sampling(_Config) ->
    ok = application:set_env(opentelemetry_nova, trust_incoming_context, false),
    run_request(~"GET", ~"/metrics", #{~"traceparent" => ?TRACEPARENT}),
    receive
        {span, #span{} = Span} -> ct:fail({should_not_sample, Span})
    after 200 ->
        ok
    end.

untrusted_context_starts_new_trace_with_link(_Config) ->
    ok = application:set_env(opentelemetry_nova, trust_incoming_context, false),
    run_request(~"GET", ~"/users", #{~"traceparent" => ?TRACEPARENT, ~"baggage" => ~"k=v"}),
    receive
        {span, #span{trace_id = TraceId, parent_span_id = Parent, links = Links}} ->
            ?assertNotEqual(?TRACE_ID, TraceId),
            ?assertEqual(undefined, Parent),
            ?assertMatch(
                [#link{trace_id = ?TRACE_ID, span_id = ?SPAN_ID}], otel_links:list(Links)
            )
    after 5000 ->
        ct:fail(timeout_waiting_for_span)
    end.

map_config_delegates_to_sampler(_Config) ->
    Drop = otel_sampler:new(
        {otel_nova_sampler, #{exclude => [~"GET /metrics"], sampler => {trace_id_ratio_based, 0.0}}}
    ),
    Keep = otel_sampler:new(
        {otel_nova_sampler, #{exclude => [~"GET /metrics"], sampler => {trace_id_ratio_based, 1.0}}}
    ),
    ?assertMatch({?DROP, _, _}, decide(Drop, ~"/users")),
    ?assertMatch({?RECORD_AND_SAMPLE, _, _}, decide(Keep, ~"/users")),
    ?assertMatch({?DROP, _, _}, decide(Keep, ~"/metrics")).

map_config_defaults_to_always_on(_Config) ->
    Sampler = otel_sampler:new({otel_nova_sampler, #{exclude => [~"GET /metrics"]}}),
    ?assertMatch({?RECORD_AND_SAMPLE, _, _}, decide(Sampler, ~"/users")),
    ?assertMatch({?DROP, _, _}, decide(Sampler, ~"/metrics")),
    ?assertMatch(
        {?RECORD_AND_SAMPLE, _, _}, decide(otel_sampler:new({otel_nova_sampler, #{}}), ~"/metrics")
    ).

description_includes_delegate(_Config) ->
    Sampler = otel_sampler:new(
        {otel_nova_sampler, #{exclude => [~"GET /metrics"], sampler => {trace_id_ratio_based, 0.5}}}
    ),
    Description = otel_sampler:description(Sampler),
    ?assertNotEqual(nomatch, string:find(Description, ~"TraceIdRatioBased")),
    ?assertNotEqual(nomatch, string:find(Description, ~"GET /metrics")),
    ?assertNotEqual(
        nomatch,
        string:find(
            otel_sampler:description(otel_sampler:new({otel_nova_sampler, []})), ~"AlwaysOn"
        )
    ).

trust_callback_decides_per_request(_Config) ->
    ok = application:set_env(
        opentelemetry_nova, trust_incoming_context, {otel_nova_test_controller, trust_internal}
    ),
    run_request(~"GET", ~"/users", #{~"traceparent" => ?TRACEPARENT, ~"x-internal" => ~"yes"}),
    receive
        {span, #span{trace_id = Trusted}} -> ?assertEqual(?TRACE_ID, Trusted)
    after 5000 ->
        ct:fail(timeout_waiting_for_span)
    end,
    run_request(~"GET", ~"/users", #{~"traceparent" => ?TRACEPARENT}),
    receive
        {span, #span{trace_id = Untrusted, links = Links}} ->
            ?assertNotEqual(?TRACE_ID, Untrusted),
            ?assertMatch([#link{trace_id = ?TRACE_ID}], otel_links:list(Links))
    after 5000 ->
        ct:fail(timeout_waiting_for_span)
    end.

crashing_trust_callback_does_not_trust(_Config) ->
    [
        begin
            ok = application:set_env(opentelemetry_nova, trust_incoming_context, Setting),
            run_request(~"GET", ~"/users", #{~"traceparent" => ?TRACEPARENT}),
            receive
                {span, #span{trace_id = TraceId}} -> ?assertNotEqual(?TRACE_ID, TraceId, Setting)
            after 5000 ->
                ct:fail(timeout_waiting_for_span)
            end
        end
     || Setting <- [
            {otel_nova_test_controller, crashing_trust},
            {otel_nova_test_controller, missing},
            {otel_nova_test_controller, span_name},
            "true"
        ]
    ].

decide(Sampler, Path) ->
    otel_sampler:should_sample(
        Sampler,
        otel_ctx:new(),
        ?TRACE_ID,
        otel_links:new([], 128, 128, 128),
        ~"HTTP GET",
        ?SPAN_KIND_SERVER,
        #{'http.request.method' => ~"GET", 'url.path' => Path}
    ).

run_request(Method, Path) ->
    run_request(Method, Path, #{}).

run_request(Method, Path, Headers) ->
    Req0 = otel_nova_test_util:stream_req(Method, Path),
    Req = Req0#{qs => ~"probe=1", headers => Headers#{~"host" => ~"localhost:8080"}},
    {[], State0} = otel_nova_stream_h:init(1, Req, #{stream_handlers => []}),
    {[], State} = otel_nova_stream_h:info(1, {response, 200, #{}, ~"ok"}, State0),
    otel_nova_stream_h:terminate(1, normal, State).
