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
  `false`.
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
    opts :: otel_nova_attributes:options() | undefined,
    response_header_attrs :: map(),
    extra_metric_attrs :: map()
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
        extra_metric_attrs = #{}
    },
    {Req1, State} = safely(init, fun() -> start_request(Req, State0) end, {Req, State0}),
    {Commands, Next} = cowboy_stream:init(StreamID, Req1, Opts),
    {Commands, State#state{next = Next}}.

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
    State = safely(info, fun() -> capture_response_headers(Info, State1) end, State1),
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
early_error(StreamID, Reason, PartialReq, Resp, Opts) ->
    cowboy_stream:early_error(StreamID, Reason, PartialReq, Resp, Opts).

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
        span_name(Method),
        #{kind => ?SPAN_KIND_SERVER, attributes => Attrs, links => Links}
    ),
    Ctx = otel_tracer:set_current_span(ParentCtx, SpanCtx),
    safely(
        init, fun() -> ?updown_counter_add('http.server.active_requests', 1, MetricAttrs) end, ok
    ),
    {Req#{?CTX_KEY => Ctx}, State#state{
        span_ctx = SpanCtx, metric_attrs = MetricAttrs, opts = Opts
    }}.

span_name(~"_OTHER") -> ~"HTTP";
span_name(Method) -> <<"HTTP ", Method/binary>>.

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
observe(_Info, State) ->
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
    _ = otel_span:end_span(SpanCtx),
    safely(terminate, fun() -> end_active_request(State) end, ok),
    safely(terminate, fun() -> record_metrics(State) end, ok).

end_active_request(#state{metric_attrs = MetricAttrs}) ->
    ?updown_counter_add('http.server.active_requests', -1, MetricAttrs).

record_metrics(#state{
    status_code = StatusCode,
    http_route = Route,
    req_start = ReqStart,
    req_body_length = ReqBodyLen,
    resp_body_length = RespBodyLen,
    metric_attrs = MetricAttrs,
    error_type = Tag,
    extra_metric_attrs = Extra
}) ->
    Duration =
        erlang:convert_time_unit(erlang:monotonic_time() - ReqStart, native, microsecond) / 1000000,
    Attrs = otel_nova_attributes:final_metric(MetricAttrs, Route, StatusCode, Tag, Extra),
    ?histogram_record('http.server.request.duration', Duration, Attrs),
    record_if_positive('http.server.request.body.size', ReqBodyLen, Attrs),
    record_if_positive('http.server.response.body.size', RespBodyLen, Attrs),
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

body_size({sendfile, _Offset, Length, _Path}) -> Length;
body_size(Body) -> iolist_size(Body).

record_if_positive(Name, Value, Attrs) when Value > 0 ->
    ?histogram_record(Name, Value, Attrs);
record_if_positive(_, _, _) ->
    ok.

safely(Callback, Fun, Default) ->
    try
        Fun()
    catch
        Class:Reason:Stacktrace ->
            otel_nova_http:log_failure(Callback, Class, Reason, Stacktrace),
            Default
    end.
