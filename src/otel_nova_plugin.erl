-module(otel_nova_plugin).
-moduledoc """
Nova plugin that links the request process to the server span created by
`otel_nova_stream_h` and enriches that span with routing information.

Register it as the first `pre_request` plugin. It attaches the trace context
the stream handler stored in the Req, so plugins that run after it,
controllers and libraries such as opentelemetry_kura create child spans of
the server span. It also sets `http.route`, the span name and the `nova.*`
attributes, adds `trace_id`/`span_id` to the logger process metadata and
reports the route to the stream handler for the HTTP metrics.

`http.route` is the route template. Nova versions that store the matched
template in the Req under `nova_route` provide it directly. Otherwise it is
rebuilt from the path: bound segments become `:name` and the tail matched by
a catch-all such as `/assets/[...]` becomes `[...]`. When the template cannot
be determined the attribute is left out rather than set to
the raw path, so the number of metric series stays bounded. Instrumentation
failures are logged without request data and never fail the request.

Also register it as a `post_request` plugin. When Nova reports a controller
or plugin crash in `crash_info`, it sets `error.type` on the server span to
the crash reason (the reason atom, the tag of a tagged tuple, or the class),
sets the span status to error and reports the tag to the stream handler,
which uses it as the `error.type` metric attribute of a 5xx response.
""".
%% Implements nova_plugin behaviour callbacks.
%% The -behaviour directive is omitted because nova is only a test dependency.

-export([pre_request/4, post_request/4, plugin_info/0]).

-include_lib("opentelemetry_api/include/opentelemetry.hrl").

pre_request(Req, Env, _Opts, State) ->
    try
        instrument(Req, Env)
    catch
        Class:Reason:Stacktrace ->
            otel_nova_stream_h:log_failure(pre_request, Class, Reason, Stacktrace)
    end,
    {ok, Req, State}.

post_request(Req, _Env, _Opts, State) ->
    try
        record_crash(Req)
    catch
        Class:Reason:Stacktrace ->
            otel_nova_stream_h:log_failure(post_request, Class, Reason, Stacktrace)
    end,
    {ok, Req, State}.

plugin_info() ->
    #{
        title => <<"otel_nova_plugin">>,
        version => <<"0.1.0">>,
        url => <<"https://github.com/novaframework/opentelemetry_nova">>,
        authors => [<<"OpenTelemetry Nova contributors">>],
        description => <<"Enriches OpenTelemetry spans with Nova routing information">>
    }.

%% Internal

instrument(Req, Env) ->
    attach_context(Req),
    record_crash(Req),
    SpanCtx = otel_tracer:current_span_ctx(),
    case route(Req) of
        undefined ->
            ok;
        Route ->
            Method = otel_nova_http:span_method(otel_nova_http:method(cowboy_req:method(Req))),
            annotate(SpanCtx, #{'http.route' => Route}),
            report(otel_nova_route, Route, Req),
            rename(SpanCtx, <<Method/binary, " ", Route/binary>>)
    end,
    case extract_callback_info(Env) of
        {App, Controller, Action} ->
            annotate(SpanCtx, #{
                'nova.app' => atom_to_binary(App, utf8),
                'nova.controller' => atom_to_binary(Controller, utf8),
                'nova.action' => atom_to_binary(Action, utf8)
            });
        undefined ->
            ok
    end,
    enrich_logger_metadata(SpanCtx).

annotate(undefined, _Attrs) ->
    ok;
annotate(SpanCtx, Attrs) ->
    _ = otel_span:set_attributes(SpanCtx, Attrs),
    ok.

rename(undefined, _Name) ->
    ok;
rename(SpanCtx, Name) ->
    _ = otel_span:update_name(SpanCtx, Name),
    ok.

%% Nova stores `crash_info` in the Req when a controller or plugin crashes and
%% renders a status page instead. A map with a `reason` comes from a crash; a
%% `throw({Status, Reason})` stores the bare Reason and is not an error.
record_crash(Req = #{crash_info := #{reason := Reason} = CrashInfo}) ->
    Tag = error_tag(Reason, maps:get(class, CrashInfo, undefined)),
    SpanCtx = request_span_ctx(Req),
    annotate(SpanCtx, #{'error.type' => Tag}),
    set_error(SpanCtx, Tag),
    report(otel_nova_error_type, Tag, Req);
record_crash(_Req) ->
    ok.

error_tag(Reason, _Class) when is_atom(Reason) ->
    atom_to_binary(Reason);
error_tag(Reason, _Class) when
    is_tuple(Reason), tuple_size(Reason) > 0, is_atom(element(1, Reason))
->
    atom_to_binary(element(1, Reason));
error_tag(_Reason, Class) when is_atom(Class), Class =/= undefined ->
    atom_to_binary(Class);
error_tag(_Reason, _Class) ->
    ~"_OTHER".

request_span_ctx(#{otel_nova_ctx := Ctx}) ->
    otel_tracer:current_span_ctx(Ctx);
request_span_ctx(_Req) ->
    otel_tracer:current_span_ctx().

set_error(undefined, _Tag) ->
    ok;
set_error(SpanCtx, Tag) ->
    _ = otel_span:set_status(SpanCtx, ?OTEL_STATUS_ERROR, Tag),
    ok.

attach_context(#{otel_nova_ctx := Ctx}) ->
    _ = otel_ctx:attach(Ctx),
    ok;
attach_context(_Req) ->
    ok.

report(Tag, Value, Req = #{otel_nova_ctx := _}) ->
    cowboy_req:cast({Tag, Value}, Req);
report(_Tag, _Value, _Req) ->
    ok.

enrich_logger_metadata(undefined) ->
    ok;
enrich_logger_metadata(SpanCtx) ->
    case otel_span:is_valid(SpanCtx) of
        true ->
            logger:update_process_metadata(#{
                trace_id => otel_span:hex_trace_id(SpanCtx),
                span_id => otel_span:hex_span_id(SpanCtx)
            });
        false ->
            ok
    end.

%% Nova versions that put the matched template in the Req under `nova_route`
%% are trusted as is. Otherwise the template is rebuilt from the request path
%% the way Nova's router sees it: empty, "." and ".." segments resolved, the
%% catch-all tail that Nova reports as `pathinfo` folded into `[...]` and bound
%% segments replaced by their names. A label is only emitted when that is possible, so the set of
%% `http.route` values stays bounded by the route table.
route(Req = #{nova_route := Route}) when is_binary(Route) ->
    case otel_nova_http:is_utf8(Route) of
        true -> Route;
        false -> rebuild_route(Req)
    end;
route(Req) ->
    rebuild_route(Req).

rebuild_route(Req = #{path := Path}) when is_binary(Path) ->
    Segments = canonical_segments(Path),
    case split_pathinfo(Segments, Req) of
        {ok, Fixed, Tail} ->
            Template = replace_bindings(Fixed, maps:get(bindings, Req, #{})) ++ Tail,
            valid_route(join_route(Template));
        error ->
            undefined
    end;
rebuild_route(_Req) ->
    undefined.

join_route([]) -> ~"/";
join_route(Segments) -> <<<<"/", S/binary>> || S <- Segments>>.

valid_route(Route) ->
    case otel_nova_http:is_utf8(Route) of
        true -> Route;
        false -> undefined
    end.

canonical_segments(Path) ->
    canonicalise([S || S <- binary:split(Path, ~"/", [global]), S =/= ~""], []).

canonicalise([], Acc) ->
    lists:reverse(Acc);
canonicalise([~"." | Tl], Acc) ->
    canonicalise(Tl, Acc);
canonicalise([~".." | Tl], []) ->
    canonicalise(Tl, []);
canonicalise([~".." | Tl], [_ | Acc]) ->
    canonicalise(Tl, Acc);
canonicalise([Segment | Tl], Acc) ->
    canonicalise(Tl, [Segment | Acc]).

split_pathinfo(Segments, #{extra_state := #{pathinfo := PathInfo}}) when
    is_list(PathInfo), length(PathInfo) =< length(Segments)
->
    case lists:split(length(Segments) - length(PathInfo), Segments) of
        {Fixed, PathInfo} -> {ok, Fixed, [~"[...]"]};
        _ -> error
    end;
split_pathinfo(_Segments, #{extra_state := #{pathinfo := _}}) ->
    error;
split_pathinfo(Segments, _Req) ->
    {ok, Segments, []}.

replace_bindings(Segments, Bindings) when is_map(Bindings), map_size(Bindings) > 0 ->
    Names = #{Value => Name || Name := Value <- Bindings},
    [
        case Names of
            #{Segment := Name} -> <<":", Name/binary>>;
            #{} -> Segment
        end
     || Segment <- Segments
    ];
replace_bindings(Segments, _Bindings) ->
    Segments.

extract_callback_info(#{app := App, callback := Fun}) when is_function(Fun) ->
    case {erlang:fun_info(Fun, module), erlang:fun_info(Fun, name)} of
        {{module, Mod}, {name, Name}} ->
            {App, Mod, Name};
        _ ->
            undefined
    end;
extract_callback_info(_) ->
    undefined.
