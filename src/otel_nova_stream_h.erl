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
stream.
""".
-behavior(cowboy_stream).

-export([init/3, data/4, info/3, terminate/3, early_error/5]).

-include_lib("kernel/include/logger.hrl").
-include_lib("opentelemetry_api/include/otel_tracer.hrl").
-include_lib("opentelemetry_api/include/opentelemetry.hrl").
-include_lib("opentelemetry_api_experimental/include/otel_meter.hrl").

-record(state, {
    next :: undefined | {module(), term()},
    span_ctx :: opentelemetry:span_ctx() | undefined,
    status_code :: integer() | undefined,
    http_route :: binary() | undefined,
    req_start :: integer(),
    req_body_length :: non_neg_integer(),
    resp_body_length :: non_neg_integer(),
    metric_attrs :: map()
}).

-define(CTX_KEY, otel_nova_ctx).

-spec init(cowboy_stream:streamid(), cowboy_req:req(), cowboy:opts()) ->
    {cowboy_stream:commands(), #state{}}.
init(StreamID, Req, Opts) ->
    State0 = #state{
        req_start = erlang:monotonic_time(),
        req_body_length = 0,
        resp_body_length = 0,
        metric_attrs = #{}
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
info(StreamID, Info, State0) ->
    State = safely(info, fun() -> observe(Info, State0) end, State0),
    {Commands, Next} = cowboy_stream:info(StreamID, Info, State#state.next),
    {Commands, State#state{next = Next}}.

-spec terminate(cowboy_stream:streamid(), cowboy_stream:reason(), #state{}) -> any().
terminate(StreamID, Reason, State = #state{next = Next}) ->
    safely(terminate, fun() -> finish_request(State) end, ok),
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
    Headers = cowboy_req:headers(Req),
    ParentCtx = otel_propagator_text_map:extract_to(otel_ctx:new(), maps:to_list(Headers)),
    Method = cowboy_req:method(Req),
    MetricAttrs = metric_attributes(Req),
    SpanCtx = otel_tracer:start_span(
        ParentCtx,
        ?current_tracer,
        <<"HTTP ", Method/binary>>,
        #{kind => ?SPAN_KIND_SERVER, attributes => request_attributes(Req)}
    ),
    Ctx = otel_tracer:set_current_span(ParentCtx, SpanCtx),
    ?updown_counter_add('http.server.active_requests', 1, MetricAttrs),
    {Req#{?CTX_KEY => Ctx}, State#state{span_ctx = SpanCtx, metric_attrs = MetricAttrs}}.

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

finish_request(#state{span_ctx = undefined}) ->
    ok;
finish_request(#state{
    span_ctx = SpanCtx,
    status_code = StatusCode,
    http_route = Route,
    req_start = ReqStart,
    req_body_length = ReqBodyLen,
    resp_body_length = RespBodyLen,
    metric_attrs = MetricAttrs
}) ->
    set_status_attributes(SpanCtx, StatusCode),
    _ = otel_span:end_span(SpanCtx),

    Duration =
        erlang:convert_time_unit(erlang:monotonic_time() - ReqStart, native, microsecond) / 1000000,
    FinalAttrs = maps:merge(MetricAttrs, maps:merge(route_attrs(Route), status_attrs(StatusCode))),
    ?histogram_record('http.server.request.duration', Duration, FinalAttrs),
    record_if_positive('http.server.request.body.size', ReqBodyLen, FinalAttrs),
    record_if_positive('http.server.response.body.size', RespBodyLen, FinalAttrs),
    ?updown_counter_add('http.server.active_requests', -1, MetricAttrs),
    ok.

set_status_attributes(_SpanCtx, undefined) ->
    ok;
set_status_attributes(SpanCtx, Code) when Code >= 500 ->
    _ = otel_span:set_attributes(SpanCtx, #{
        'http.response.status_code' => Code,
        'error.type' => integer_to_binary(Code)
    }),
    _ = otel_span:set_status(SpanCtx, ?OTEL_STATUS_ERROR, ~"Server error"),
    ok;
set_status_attributes(SpanCtx, Code) ->
    _ = otel_span:set_attribute(SpanCtx, 'http.response.status_code', Code),
    ok.

route_attrs(undefined) -> #{};
route_attrs(Route) -> #{'http.route' => Route}.

status_attrs(undefined) ->
    #{};
status_attrs(Code) when Code >= 500 ->
    #{'http.response.status_code' => Code, 'error.type' => integer_to_binary(Code)};
status_attrs(Code) ->
    #{'http.response.status_code' => Code}.

body_size({sendfile, _Offset, Length, _Path}) -> Length;
body_size(Body) -> iolist_size(Body).

%% Cowboy logs the arguments and state of a crashing stream handler, which
%% include the request headers and cookies. Instrumentation must never crash,
%% and what it logs carries no request data.
safely(Callback, Fun, Default) ->
    try
        Fun()
    catch
        Class:Reason:Stacktrace ->
            ?LOG_WARNING(#{
                what => otel_nova_instrumentation_failed,
                callback => Callback,
                class => Class,
                reason => reason_tag(Reason),
                stacktrace => [strip_args(Frame) || Frame <- Stacktrace]
            }),
            Default
    end.

reason_tag(Reason) when is_atom(Reason) -> Reason;
reason_tag(Reason) when is_tuple(Reason), is_atom(element(1, Reason)) -> element(1, Reason);
reason_tag(_Reason) -> unknown.

strip_args({Module, Function, Args, Location}) when is_list(Args) ->
    {Module, Function, length(Args), Location};
strip_args(Frame) ->
    Frame.

request_attributes(Req) ->
    Method = cowboy_req:method(Req),
    Path = cowboy_req:path(Req),
    Scheme = cowboy_req:scheme(Req),
    Host = cowboy_req:host(Req),
    Port = cowboy_req:port(Req),
    {PeerAddr, PeerPort} = cowboy_req:peer(Req),
    PeerBin = peer_to_binary(PeerAddr),

    Attrs0 = #{
        'http.request.method' => Method,
        'url.path' => Path,
        'url.scheme' => Scheme,
        'server.address' => Host,
        'server.port' => Port,
        'network.peer.address' => PeerBin,
        'network.peer.port' => PeerPort,
        'network.protocol.version' => protocol_version(Req),
        'client.address' => client_address(Req, PeerBin)
    },

    case cowboy_req:header(~"user-agent", Req) of
        undefined -> Attrs0;
        UA -> Attrs0#{'user_agent.original' => UA}
    end.

metric_attributes(Req) ->
    #{
        'http.request.method' => cowboy_req:method(Req),
        'url.scheme' => cowboy_req:scheme(Req),
        'server.address' => cowboy_req:host(Req),
        'server.port' => cowboy_req:port(Req)
    }.

peer_to_binary(Addr) ->
    case inet:ntoa(Addr) of
        {error, _} -> ~"unknown";
        String -> list_to_binary(String)
    end.

protocol_version(#{version := 'HTTP/1.0'}) -> ~"1.0";
protocol_version(#{version := 'HTTP/1.1'}) -> ~"1.1";
protocol_version(#{version := 'HTTP/2'}) -> ~"2";
protocol_version(#{version := 'HTTP/3'}) -> ~"3";
protocol_version(_) -> ~"1.1".

client_address(Req, PeerBin) ->
    case cowboy_req:header(~"x-forwarded-for", Req) of
        undefined ->
            case cowboy_req:header(~"forwarded", Req) of
                undefined -> PeerBin;
                Forwarded -> parse_forwarded_for(Forwarded, PeerBin)
            end;
        XFF ->
            [First | _] = binary:split(XFF, ~","),
            string:trim(First)
    end.

parse_forwarded_for(Forwarded, Default) ->
    case
        re:run(
            Forwarded,
            <<"for=\"?([^;,\"]+)\"?">>,
            [{capture, [1], binary}, caseless]
        )
    of
        {match, [Addr]} -> string:trim(Addr);
        nomatch -> Default
    end.

record_if_positive(Name, Value, Attrs) when Value > 0 ->
    ?histogram_record(Name, Value, Attrs);
record_if_positive(_, _, _) ->
    ok.
