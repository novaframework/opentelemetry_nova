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

`http.route` is the route template: bound segments become `:name` and the
tail matched by a catch-all such as `/assets/[...]` becomes `[...]`. When the
template cannot be determined the attribute is left out rather than set to
the raw path, so the number of metric series stays bounded.
""".
%% Implements nova_plugin behaviour callbacks.
%% The -behaviour directive is omitted because nova is only a test dependency.

-export([pre_request/4, post_request/4, plugin_info/0]).

-include_lib("opentelemetry_api/include/otel_tracer.hrl").

pre_request(Req, Env, _Opts, State) ->
    instrument(Req, Env),
    {ok, Req, State}.

post_request(Req, _Env, _Opts, State) ->
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
    case route(Req) of
        undefined ->
            ok;
        Route ->
            Method = cowboy_req:method(Req),
            ?set_attribute('http.route', Route),
            report_route(Route, Req),
            ?update_name(<<Method/binary, " ", Route/binary>>)
    end,
    case extract_callback_info(Env) of
        {App, Controller, Action} ->
            ?set_attributes(#{
                'nova.app' => atom_to_binary(App, utf8),
                'nova.controller' => atom_to_binary(Controller, utf8),
                'nova.action' => atom_to_binary(Action, utf8)
            });
        undefined ->
            ok
    end,
    enrich_logger_metadata(?current_span_ctx).

attach_context(#{otel_nova_ctx := Ctx}) ->
    _ = otel_ctx:attach(Ctx),
    ok;
attach_context(_Req) ->
    ok.

report_route(Route, Req = #{otel_nova_ctx := _}) ->
    cowboy_req:cast({otel_nova_route, Route}, Req);
report_route(_Route, _Req) ->
    ok.

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

%% The route template is rebuilt from the request path the way Nova's router
%% sees it: empty, "." and ".." segments resolved, the catch-all tail that Nova
%% reports as `pathinfo` folded into `[...]` and bound segments replaced by
%% their names. A label is only emitted when that is possible, so the set of
%% `http.route` values stays bounded by the route table.
route(Req = #{path := Path}) when is_binary(Path) ->
    Segments = canonical_segments(Path),
    case split_pathinfo(Segments, Req) of
        {ok, Fixed, Tail} ->
            Template = replace_bindings(Fixed, maps:get(bindings, Req, #{})) ++ Tail,
            iolist_to_binary([<<"/">> | lists:join(<<"/">>, Template)]);
        error ->
            undefined
    end;
route(_Req) ->
    undefined.

canonical_segments(Path) ->
    canonicalise([S || S <- binary:split(Path, <<"/">>, [global]), S =/= <<>>], []).

canonicalise([], Acc) ->
    lists:reverse(Acc);
canonicalise([<<".">> | Tl], Acc) ->
    canonicalise(Tl, Acc);
canonicalise([<<"..">> | Tl], []) ->
    canonicalise(Tl, []);
canonicalise([<<"..">> | Tl], [_ | Acc]) ->
    canonicalise(Tl, Acc);
canonicalise([Segment | Tl], Acc) ->
    canonicalise(Tl, [Segment | Acc]).

split_pathinfo(Segments, #{extra_state := #{pathinfo := PathInfo}}) when
    is_list(PathInfo), length(PathInfo) =< length(Segments)
->
    case lists:split(length(Segments) - length(PathInfo), Segments) of
        {Fixed, PathInfo} -> {ok, Fixed, [<<"[...]">>]};
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
