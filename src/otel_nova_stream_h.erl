-module(otel_nova_stream_h).
-behavior(cowboy_stream).

-export([init/3, data/4, info/3, terminate/3, early_error/5]).

-include_lib("opentelemetry_api/include/otel_tracer.hrl").
-include_lib("opentelemetry_api/include/opentelemetry.hrl").
-include_lib("opentelemetry_api_experimental/include/otel_meter.hrl").

-record(state, {
    next :: any(),
    span_ctx :: opentelemetry:span_ctx() | undefined,
    otel_ctx :: otel_ctx:t() | undefined,
    status_code :: integer() | undefined,
    req_start :: integer(),
    req_body_length :: non_neg_integer(),
    resp_body_length :: non_neg_integer(),
    metric_attrs :: map()
}).

-spec init(cowboy_stream:streamid(), cowboy_req:req(), cowboy:opts()) ->
    {cowboy_stream:commands(), #state{}}.
init(StreamID, Req, Opts) ->
    Headers = cowboy_req:headers(Req),
    otel_propagator_text_map:extract(maps:to_list(Headers)),

    Method = cowboy_req:method(Req),
    SpanName = <<"HTTP ", Method/binary>>,
    Attrs = request_attributes(Req),

    SpanCtx = ?start_span(SpanName, #{kind => ?SPAN_KIND_SERVER, attributes => Attrs}),
    ?set_current_span(SpanCtx),
    OtelCtx = otel_ctx:get_current(),

    MetricAttrs = metric_attributes(Req),
    ?updown_counter_add('http.server.active_requests', 1, MetricAttrs),

    {Commands, Next} = cowboy_stream:init(StreamID, Req, Opts),
    {Commands, #state{next = Next,
                      span_ctx = SpanCtx,
                      otel_ctx = OtelCtx,
                      req_start = erlang:monotonic_time(),
                      req_body_length = 0,
                      resp_body_length = 0,
                      metric_attrs = MetricAttrs}}.

-spec data(cowboy_stream:streamid(), cowboy_stream:fin(), cowboy_req:resp_body(), #state{}) ->
    {cowboy_stream:commands(), #state{}}.
data(StreamID, IsFin, Data, State = #state{next = Next, req_body_length = Acc}) ->
    {Commands, Next0} = cowboy_stream:data(StreamID, IsFin, Data, Next),
    {Commands, State#state{next = Next0, req_body_length = Acc + iolist_size(Data)}}.

-spec info(cowboy_stream:streamid(), any(), #state{}) ->
    {cowboy_stream:commands(), #state{}}.
info(StreamID, {response, Code, _Headers, Body} = Info, State = #state{next = Next})
  when is_integer(Code) ->
    {Commands, Next0} = cowboy_stream:info(StreamID, Info, Next),
    {Commands, State#state{next = Next0, status_code = Code,
                           resp_body_length = iolist_size(Body)}};
info(StreamID, {headers, Code, _Headers} = Info, State = #state{next = Next})
  when is_integer(Code) ->
    {Commands, Next0} = cowboy_stream:info(StreamID, Info, Next),
    {Commands, State#state{next = Next0, status_code = Code}};
info(StreamID, {error_response, Code, _Headers, Body} = Info, State = #state{next = Next})
  when is_integer(Code) ->
    {Commands, Next0} = cowboy_stream:info(StreamID, Info, Next),
    {Commands, State#state{next = Next0, status_code = Code,
                           resp_body_length = iolist_size(Body)}};
info(StreamID, Info, State = #state{next = Next}) ->
    {Commands, Next0} = cowboy_stream:info(StreamID, Info, Next),
    {Commands, State#state{next = Next0}}.

-spec terminate(cowboy_stream:streamid(), cowboy_stream:reason(), #state{}) -> any().
terminate(StreamID, Reason, #state{next = Next, span_ctx = SpanCtx, otel_ctx = OtelCtx,
                                    status_code = StatusCode, req_start = ReqStart,
                                    req_body_length = ReqBodyLen,
                                    resp_body_length = RespBodyLen,
                                    metric_attrs = MetricAttrs}) ->
    otel_ctx:attach(OtelCtx),
    ?set_current_span(SpanCtx),

    case StatusCode of
        undefined ->
            ok;
        Code ->
            ?set_attribute('http.response.status_code', Code),
            maybe_set_error_status(Code)
    end,

    ?end_span(),
    otel_ctx:clear(),

    %% Record metrics
    EndTime = erlang:monotonic_time(),
    Duration = erlang:convert_time_unit(EndTime - ReqStart, native, millisecond) / 1000,
    FinalAttrs = case StatusCode of
        undefined -> MetricAttrs;
        SC -> MetricAttrs#{'http.response.status_code' => SC}
    end,
    ?histogram_record('http.server.request.duration', Duration, FinalAttrs),
    record_if_positive('http.server.request.body.size', ReqBodyLen, FinalAttrs),
    record_if_positive('http.server.response.body.size', RespBodyLen, FinalAttrs),
    ?updown_counter_add('http.server.active_requests', -1, MetricAttrs),

    cowboy_stream:terminate(StreamID, Reason, Next).

-spec early_error(cowboy_stream:streamid(), cowboy_stream:reason(),
                  cowboy_stream:partial_req(), Resp, cowboy:opts()) -> Resp
    when Resp :: cowboy_stream:resp_command().
early_error(StreamID, Reason, PartialReq, Resp, Opts) ->
    cowboy_stream:early_error(StreamID, Reason, PartialReq, Resp, Opts).

%% Internal

request_attributes(Req) ->
    Method = cowboy_req:method(Req),
    Path = cowboy_req:path(Req),
    Scheme = cowboy_req:scheme(Req),
    Host = cowboy_req:host(Req),
    Port = cowboy_req:port(Req),
    {PeerAddr, PeerPort} = cowboy_req:peer(Req),

    Attrs = #{
        'http.request.method' => Method,
        'url.path' => Path,
        'url.scheme' => Scheme,
        'server.address' => Host,
        'server.port' => Port,
        'network.peer.address' => peer_to_binary(PeerAddr),
        'network.peer.port' => PeerPort
    },

    case cowboy_req:header(<<"user-agent">>, Req) of
        undefined -> Attrs;
        UA -> Attrs#{'user_agent.original' => UA}
    end.

metric_attributes(Req) ->
    #{
        'http.request.method' => cowboy_req:method(Req),
        'url.scheme' => cowboy_req:scheme(Req),
        'server.address' => cowboy_req:host(Req),
        'server.port' => cowboy_req:port(Req)
    }.

peer_to_binary(Addr) when is_tuple(Addr) ->
    list_to_binary(inet:ntoa(Addr));
peer_to_binary(Addr) ->
    Addr.

maybe_set_error_status(Code) when Code >= 500 ->
    ?set_status(?OTEL_STATUS_ERROR, <<"Server error">>);
maybe_set_error_status(_) ->
    ok.

record_if_positive(Name, Value, Attrs) when Value > 0 ->
    ?histogram_record(Name, Value, Attrs);
record_if_positive(_, _, _) ->
    ok.
