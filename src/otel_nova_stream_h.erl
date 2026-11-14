-module(otel_nova_stream_h).
-moduledoc """
Cowboy stream handler that creates the HTTP server span and records the HTTP
server metrics.

Cowboy runs stream handlers in the connection process, while Nova middleware,
plugins and controllers run in a separate request process. The handler
therefore never touches the connection process context. It stores the request
context in the Req under `otel_nova_ctx`, and `otel_nova_plugin` attaches it in
the request process, so spans created there are children of the server span.

Instrumentation failures are logged without request data and never crash the
stream, and the server span is started even when building its attributes
fails.

A request Cowboy rejects before it creates a stream (a malformed or oversized
request line or header block, headers that arrive too slowly) reaches
`early_error/5` only. It is recorded as one `http.server.request.duration`
measurement with the normalised method (`_OTHER` when none was parsed), the
status code and `error.type`, and as a root server span without header or
query attributes. `http.server.active_requests` is not touched.

A protocol upgrade such as a WebSocket is recorded as the handshake: the span
ends and the duration is recorded when Cowboy answers the upgrade, with status
`101` over HTTP/1.1 and `200` over HTTP/2, not when the socket closes.

Options, read from the `opentelemetry_nova` application environment:

* `trust_forwarded_for` (default `false`): take `client.address` from the
  first `X-Forwarded-For` entry or the `Forwarded` header when it is a valid
  IP address. Otherwise it is the peer address. A list of header names out of
  `~"forwarded"`, `~"x-forwarded-for"` and `~"x-real-ip"` picks the headers
  and their priority.
* `metric_server_hosts` (default unset): a list of hosts recorded as the
  `server.address` metric attribute, together with the listener port as
  `server.port`. Other hosts are recorded as `_OTHER`. When unset, neither
  attribute is recorded on metrics.
* `trust_incoming_context` (default `true`): continue the trace context and
  baggage the client sends. When `false` the server span starts a new trace
  and links to the remote parent. `{Mod, Fun}` decides per request:
  `Mod:Fun(Req)` returns `true` to trust it; any other result or a crash means
  `false`. It runs in the connection process and must do no I/O.
* `url_query` (default `false`): set `url.query`, with the values of
  secret-looking keys replaced by `REDACTED`.
* `url_query_redact_keys` (default `[]`): more key fragments to redact.
* `request_headers` and `response_headers` (default `[]`): header names,
  lowercased, recorded as `http.request.header.<name>` and
  `http.response.header.<name>` span attributes.
* `sensitive_headers` (default `[]`): captured headers whose value is recorded
  as `REDACTED`, in addition to `authorization`, `proxy-authorization`,
  `cookie`, `set-cookie`, `x-api-key`, `x-auth-token`, `x-csrf-token` and
  `x-amz-security-token`.
* `opt_in_attributes` (default `[]`): any of `'client.port'`,
  `'network.local.address'`, `'network.local.port'`, `'network.transport'`,
  `'http.request.body.size'` and `'http.response.body.size'`.

The method is normalised to one of the nine standard methods or `_OTHER`, on
both spans and metrics. Spans keep the sent value as
`http.request.method_original` when it differs.
""".
-behavior(cowboy_stream).

-export([init/3, data/4, info/3, terminate/3, early_error/5]).

-include_lib("opentelemetry_api/include/otel_tracer.hrl").
-include_lib("opentelemetry_api/include/opentelemetry.hrl").
-include_lib("opentelemetry_api_experimental/include/otel_meter.hrl").

-define(CTX_KEY, otel_nova_ctx).

-record(state, {
    next :: undefined | {module(), term()},
    span_ctx :: opentelemetry:span_ctx() | undefined,
    status_code :: integer() | undefined,
    http_route :: binary() | undefined,
    req_start :: integer(),
    req_body_length :: non_neg_integer(),
    resp_body_length :: non_neg_integer(),
    metric_attrs :: map(),
    error_type :: binary() | undefined,
    opts :: map() | undefined,
    response_header_attrs :: map(),
    extra_metric_attrs :: map(),
    version :: cowboy:http_version() | undefined,
    upgrade = false :: boolean()
}).

-spec init(cowboy_stream:streamid(), cowboy_req:req(), cowboy:opts()) ->
    {cowboy_stream:commands(), #state{}}.
init(StreamID, Req, Opts) ->
    State0 = #state{
        req_start = erlang:monotonic_time(),
        req_body_length = 0,
        resp_body_length = 0,
        metric_attrs = #{},
        response_header_attrs = #{},
        extra_metric_attrs = #{},
        version = maps:get(version, Req, undefined)
    },
    {Req1, State} = safely(init, fun() -> start_request(Req, State0) end, {Req, State0}),
    try cowboy_stream:init(StreamID, Req1, Opts) of
        {Commands, Next} ->
            {Commands, State#state{next = Next}}
    catch
        Class:Reason:Stacktrace ->
            safely(init, fun() -> finish_request(stream_failure(internal_error, State)) end, ok),
            erlang:raise(Class, Reason, Stacktrace)
    end.

-spec data(cowboy_stream:streamid(), cowboy_stream:fin(), binary(), #state{}) ->
    {cowboy_stream:commands(), #state{}}.
data(StreamID, IsFin, Data, State0 = #state{req_body_length = Acc}) ->
    State = State0#state{req_body_length = Acc + body_size(Data)},
    {Commands, Next} = cowboy_stream:data(StreamID, IsFin, Data, State#state.next),
    {Commands, State#state{next = Next}}.

-spec info(cowboy_stream:streamid(), any(), #state{}) ->
    {cowboy_stream:commands(), #state{}}.
info(_StreamID, {otel_nova_route, Route}, State) when is_binary(Route) ->
    {[], State#state{http_route = Route}};
info(_StreamID, {otel_nova_error_type, Tag}, State) when is_binary(Tag) ->
    {[], State#state{error_type = Tag}};
info(_StreamID, {otel_nova_metric_attributes, Attrs}, State) when is_map(Attrs) ->
    {[], safely(info, fun() -> add_metric_attributes(Attrs, State) end, State)};
info(StreamID, Info, State0) ->
    State1 = safely(info, fun() -> observe(Info, State0) end, State0),
    State2 = safely(info, fun() -> capture_response_headers(Info, State1) end, State1),
    State = safely(info, fun() -> finish_upgrade(Info, State2) end, State2),
    {Commands, Next} = cowboy_stream:info(StreamID, Info, State#state.next),
    {Commands, State#state{next = Next}}.

-spec terminate(cowboy_stream:streamid(), cowboy_stream:reason(), #state{}) -> any().
terminate(StreamID, Reason, State = #state{next = Next}) ->
    safely(terminate, fun() -> finish_request(stream_failure(Reason, State)) end, ok),
    cowboy_stream:terminate(StreamID, Reason, Next).

-spec early_error(
    cowboy_stream:streamid(),
    cowboy_stream:reason(),
    cowboy_stream:partial_req(),
    Resp,
    cowboy:opts()
) -> Resp when
    Resp :: cowboy_stream:resp_command().
early_error(StreamID, Reason, PartialReq, Resp0, Opts) ->
    Start = erlang:monotonic_time(),
    Resp = cowboy_stream:early_error(StreamID, Reason, PartialReq, Resp0, Opts),
    safely(early_error, fun() -> record_early_error(Reason, PartialReq, Resp, Start) end, ok),
    Resp.

%% Internal

start_request(Req, State) ->
    Opts = otel_nova_attributes:options(),
    Method = safely(init, fun() -> otel_nova_http:method(cowboy_req:method(Req)) end, ~"_OTHER"),
    Attrs = otel_nova_attributes:span(Req, Method, Opts),
    MetricAttrs = safely(init, fun() -> otel_nova_attributes:metric(Req, Method, Opts) end, #{
        'http.request.method' => Method
    }),
    {ParentCtx, Links} = safely(init, fun() -> parent(Req, Opts) end, {otel_ctx:new(), []}),
    SpanCtx = otel_tracer:start_span(
        ParentCtx,
        ?current_tracer,
        otel_nova_http:span_method(Method),
        #{kind => ?SPAN_KIND_SERVER, attributes => Attrs, links => Links}
    ),
    Ctx = otel_tracer:set_current_span(ParentCtx, SpanCtx),
    safely(
        init, fun() -> ?updown_counter_add('http.server.active_requests', 1, MetricAttrs) end, ok
    ),
    {Req#{?CTX_KEY => Ctx}, State#state{
        span_ctx = SpanCtx, metric_attrs = MetricAttrs, opts = Opts
    }}.

%% Cowboy answers a request it cannot parse without creating a stream, so
%% init/3 and terminate/3 never run and http.server.active_requests is not
%% touched. The span is a root: the headers of such a request are not trusted
%% for trace context. An internal_error is the 500 Cowboy sends over HTTP/1.1
%% after a stream handler's init/3 raised, which init/3 has already recorded.
record_early_error({internal_error, _, _}, _PartialReq, _Resp, _Start) ->
    ok;
record_early_error(Reason, PartialReq, {response, Status, _Headers, _Body}, Start) ->
    Method = otel_nova_http:method(maps:get(method, PartialReq, ~"_OTHER")),
    Tag = stream_failure_tag(Reason),
    StatusCode =
        case Status of
            _ when is_integer(Status) -> Status;
            _ -> undefined
        end,
    SpanCtx = otel_tracer:start_span(
        otel_ctx:new(),
        ?current_tracer,
        otel_nova_http:span_method(Method),
        #{
            kind => ?SPAN_KIND_SERVER,
            attributes => otel_nova_attributes:early_error_span(PartialReq, Method)
        }
    ),
    _ = otel_span:set_attribute(SpanCtx, 'error.type', Tag),
    set_status_attributes(SpanCtx, StatusCode, Tag),
    Ctx = otel_tracer:set_current_span(otel_ctx:new(), SpanCtx),
    Attrs = otel_nova_attributes:early_error_metric(Method, StatusCode, Tag),
    safely(
        early_error,
        fun() ->
            otel_histogram:record(
                Ctx, ?current_meter, 'http.server.request.duration', seconds_since(Start), Attrs
            )
        end,
        ok
    ),
    _ = otel_span:end_span(SpanCtx),
    ok.

%% With trust_incoming_context disabled the server span is a new root, so a
%% client cannot force sampling or inject baggage, and the remote parent is
%% kept as a link.
parent(Req, Opts) ->
    Headers = maps:to_list(cowboy_req:headers(Req)),
    Remote = otel_propagator_text_map:extract_to(otel_ctx:new(), Headers),
    case trust_incoming_context(Req, Opts) of
        true ->
            {Remote, []};
        false ->
            {otel_ctx:new(), links(otel_tracer:current_span_ctx(Remote))}
    end.

trust_incoming_context(Req, #{trust_incoming_context := {Mod, Fun}}) ->
    try
        Mod:Fun(Req) =:= true
    catch
        Class:Reason:Stacktrace ->
            otel_nova_http:log_failure(trust_incoming_context, Class, Reason, Stacktrace),
            false
    end;
trust_incoming_context(_Req, #{trust_incoming_context := Trust}) ->
    Trust.

links(undefined) ->
    [];
links(RemoteSpan) ->
    [opentelemetry:link(RemoteSpan) || otel_span:is_valid(RemoteSpan)].

observe({response, Code, _Headers, Body}, State) when is_integer(Code) ->
    State#state{status_code = Code, resp_body_length = body_size(Body)};
observe({headers, Code, _Headers}, State) when is_integer(Code) ->
    State#state{status_code = Code};
observe({data, _IsFin, Data}, State = #state{resp_body_length = Acc}) ->
    State#state{resp_body_length = Acc + body_size(Data)};
observe({error_response, Code, _Headers, Body}, State) when is_integer(Code) ->
    State#state{status_code = Code, resp_body_length = body_size(Body)};
observe({switch_protocol, _Headers, cowboy_http2, _ModState}, State) ->
    State;
observe({switch_protocol, _Headers, _Mod, _ModState}, State = #state{version = 'HTTP/1.1'}) ->
    State#state{status_code = 101};
observe({switch_protocol, _Headers, _Mod, _ModState}, State) ->
    State#state{upgrade = true};
observe(_Info, State) ->
    State.

%% A protocol upgrade ends the HTTP request. Over HTTP/1.1 Cowboy terminates
%% the stream as it sends the 101, so terminate/3 finishes the request. Over
%% HTTP/2 the stream stays open for the lifetime of the upgraded protocol,
%% such as a WebSocket, so the request is finished when Cowboy sends the
%% response headers of the upgrade, and terminate/3 finds nothing left to do.
finish_upgrade({headers, _Code, _Headers}, State = #state{upgrade = true, span_ctx = SpanCtx}) when
    SpanCtx =/= undefined
->
    finish_request(State),
    State#state{span_ctx = undefined};
finish_upgrade(_Info, State) ->
    State.

capture_response_headers(_Info, State = #state{opts = undefined}) ->
    State;
capture_response_headers(_Info, State = #state{opts = #{response_headers := []}}) ->
    State;
capture_response_headers({response, _Code, Headers, _Body}, State = #state{opts = Opts}) ->
    State#state{response_header_attrs = otel_nova_attributes:response_headers(Headers, Opts)};
capture_response_headers({headers, _Code, Headers}, State = #state{opts = Opts}) ->
    State#state{response_header_attrs = otel_nova_attributes:response_headers(Headers, Opts)};
capture_response_headers(_Info, State) ->
    State.

add_metric_attributes(Attrs, State = #state{extra_metric_attrs = Extra}) ->
    State#state{extra_metric_attrs = otel_nova_attributes:add_extra_metric(Attrs, Extra)}.

%% A stream that ends without a response, such as a client that closes the
%% connection or a request timeout, is tagged with the reason Cowboy gives.
stream_failure(_Reason, State = #state{status_code = Code}) when is_integer(Code) ->
    State;
stream_failure(normal, State) ->
    State;
stream_failure(switch_protocol, State) ->
    State;
stream_failure(Reason, State = #state{span_ctx = SpanCtx}) ->
    Tag = stream_failure_tag(Reason),
    _ = otel_span:set_attribute(SpanCtx, 'error.type', Tag),
    _ = otel_span:set_status(SpanCtx, ?OTEL_STATUS_ERROR, Tag),
    State#state{error_type = Tag}.

stream_failure_tag(Reason) when is_atom(Reason) ->
    atom_to_binary(Reason);
stream_failure_tag(Reason) when
    is_tuple(Reason), tuple_size(Reason) > 0, is_atom(element(1, Reason))
->
    atom_to_binary(element(1, Reason));
stream_failure_tag(_Reason) ->
    ~"_OTHER".

finish_request(#state{span_ctx = undefined}) ->
    ok;
finish_request(
    State = #state{
        span_ctx = SpanCtx,
        status_code = StatusCode,
        error_type = Tag,
        response_header_attrs = HeaderAttrs,
        opts = Opts,
        req_body_length = ReqBodyLen,
        resp_body_length = RespBodyLen
    }
) ->
    BodySizes = otel_nova_attributes:body_sizes(Opts, ReqBodyLen, RespBodyLen),
    _ = otel_span:set_attributes(SpanCtx, maps:merge(HeaderAttrs, BodySizes)),
    set_status_attributes(SpanCtx, StatusCode, Tag),
    safely(terminate, fun() -> record_metrics(State) end, ok),
    _ = otel_span:end_span(SpanCtx),
    safely(terminate, fun() -> end_active_request(State) end, ok).

end_active_request(#state{metric_attrs = MetricAttrs}) ->
    ?updown_counter_add('http.server.active_requests', -1, MetricAttrs).

%% The connection process has no attached context, so the server span is
%% passed explicitly; with exemplars enabled the measurements then carry its
%% trace and span id.
record_metrics(#state{
    span_ctx = SpanCtx,
    status_code = StatusCode,
    http_route = Route,
    req_start = ReqStart,
    req_body_length = ReqBodyLen,
    resp_body_length = RespBodyLen,
    metric_attrs = MetricAttrs,
    error_type = Tag,
    extra_metric_attrs = Extra
}) ->
    Duration = seconds_since(ReqStart),
    Attrs = otel_nova_attributes:final_metric(MetricAttrs, Route, StatusCode, Tag, Extra),
    Ctx = otel_tracer:set_current_span(otel_ctx:new(), SpanCtx),
    otel_histogram:record(Ctx, ?current_meter, 'http.server.request.duration', Duration, Attrs),
    record_if_positive(Ctx, 'http.server.request.body.size', ReqBodyLen, Attrs),
    record_if_positive(Ctx, 'http.server.response.body.size', RespBodyLen, Attrs),
    ok.

set_status_attributes(_SpanCtx, undefined, _Tag) ->
    ok;
set_status_attributes(SpanCtx, Code, Tag) when Code >= 500 ->
    _ = otel_span:set_attributes(SpanCtx, #{
        'http.response.status_code' => Code,
        'error.type' => otel_nova_http:error_type(Code, Tag)
    }),
    Description =
        case Tag of
            undefined -> ~"Server error";
            _ -> Tag
        end,
    _ = otel_span:set_status(SpanCtx, ?OTEL_STATUS_ERROR, Description),
    ok;
set_status_attributes(SpanCtx, Code, _Tag) ->
    _ = otel_span:set_attribute(SpanCtx, 'http.response.status_code', Code),
    ok.

seconds_since(Start) ->
    erlang:convert_time_unit(erlang:monotonic_time() - Start, native, microsecond) / 1000000.

body_size({sendfile, _Offset, Length, _Path}) -> Length;
body_size(Body) -> iolist_size(Body).

record_if_positive(Ctx, Name, Value, Attrs) when Value > 0 ->
    otel_histogram:record(Ctx, ?current_meter, Name, Value, Attrs);
record_if_positive(_Ctx, _Name, _Value, _Attrs) ->
    ok.

safely(Callback, Fun, Default) ->
    try
        Fun()
    catch
        Class:Reason:Stacktrace ->
            otel_nova_http:log_failure(Callback, Class, Reason, Stacktrace),
            Default
    end.
