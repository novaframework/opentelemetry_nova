-module(otel_nova_sampler_SUITE).
-behaviour(ct_suite).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include_lib("opentelemetry_api/include/otel_tracer.hrl").
-include_lib("opentelemetry_api/include/opentelemetry.hrl").
-include_lib("opentelemetry/include/otel_span.hrl").

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
        untrusted_context_starts_new_trace_with_link
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

run_request(Method, Path) ->
    run_request(Method, Path, #{}).

run_request(Method, Path, Headers) ->
    Req0 = otel_nova_test_util:stream_req(Method, Path),
    Req = Req0#{qs => ~"probe=1", headers => Headers#{~"host" => ~"localhost:8080"}},
    {[], State0} = otel_nova_stream_h:init(1, Req, #{stream_handlers => []}),
    {[], State} = otel_nova_stream_h:info(1, {response, 200, #{}, ~"ok"}, State0),
    otel_nova_stream_h:terminate(1, normal, State).
