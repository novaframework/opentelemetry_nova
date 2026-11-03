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
""".
%% Implements nova_plugin behaviour callbacks.
%% The -behaviour directive is omitted because nova is only a test dependency.

-export([pre_request/4, post_request/4, plugin_info/0]).

-include_lib("opentelemetry_api/include/otel_tracer.hrl").

pre_request(Req, Env, _Opts, State) ->
    attach_context(Req),
    Method = cowboy_req:method(Req),
    Route = reconstruct_route(Req),

    ?set_attribute('http.route', Route),
    report_route(Route, Req),
    ?update_name(<<Method/binary, " ", Route/binary>>),

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
    enrich_logger_metadata(?current_span_ctx),
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

reconstruct_route(#{bindings := Bindings, path := Path}) when map_size(Bindings) > 0 ->
    Segments = binary:split(Path, <<"/">>, [global]),
    InverseBindings = maps:fold(
        fun(K, V, Acc) ->
            Acc#{V => K}
        end,
        #{},
        Bindings
    ),
    Replaced = [
        case maps:find(Seg, InverseBindings) of
            {ok, Key} -> <<":", Key/binary>>;
            error -> Seg
        end
     || Seg <- Segments
    ],
    iolist_to_binary(lists:join(<<"/">>, Replaced));
reconstruct_route(#{path := Path}) ->
    Path.

extract_callback_info(#{app := App, callback := Fun}) when is_function(Fun) ->
    case {erlang:fun_info(Fun, module), erlang:fun_info(Fun, name)} of
        {{module, Mod}, {name, Name}} ->
            {App, Mod, Name};
        _ ->
            undefined
    end;
extract_callback_info(_) ->
    undefined.
