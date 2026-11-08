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
* `url_query` (default `true`): set `url.query`, with the values of secret
  keys replaced by `REDACTED`.
* `url_query_redact_keys` (default `[]`): more query keys to redact.
* `request_headers` and `response_headers` (default `[]`): lowercase header
  names recorded as `http.request.header.<name>` and
  `http.response.header.<name>` span attributes.
* `sensitive_headers` (default `authorization`, `proxy-authorization`,
  `cookie`, `set-cookie`, `x-api-key`): captured headers whose value is
  recorded as `REDACTED`.
* `opt_in_attributes` (default `[]`): any of `'client.port'`,
  `'network.local.address'`, `'network.local.port'`, `'network.transport'`,
  `'http.request.body.size'` and `'http.response.body.size'`.

The method is normalised to one of the nine standard methods or `_OTHER`, on
both spans and metrics. Spans keep the sent value as
`http.request.method_original` when it differs.
""".
-behavior(cowboy_stream).

-export([init/3, data/4, info/3, terminate/3, early_error/5]).
-export([log_failure/4]).

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
    metric_attrs :: map(),
    error_type :: binary() | undefined,
    response_headers :: [binary()],
    sensitive_headers :: [binary()],
    response_header_attrs :: map(),
    opt_in :: [atom()],
    extra_metric_attrs :: map()
}).

-define(BUILTIN_METRIC_ATTRIBUTES, [
    'http.request.method',
    'url.scheme',
    'http.route',
    'http.response.status_code',
    'error.type',
    'server.address',
    'server.port'
]).

-define(SENSITIVE_HEADERS, [
    ~"authorization", ~"proxy-authorization", ~"cookie", ~"set-cookie", ~"x-api-key"
]).

-define(CTX_KEY, otel_nova_ctx).

-spec init(cowboy_stream:streamid(), cowboy_req:req(), cowboy:opts()) ->
    {cowboy_stream:commands(), #state{}}.
init(StreamID, Req, Opts) ->
    State0 = #state{
        req_start = erlang:monotonic_time(),
        req_body_length = 0,
        resp_body_length = 0,
        metric_attrs = #{},
        response_headers = [],
        sensitive_headers = ?SENSITIVE_HEADERS,
        response_header_attrs = #{},
        opt_in = [],
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
info(
    _StreamID, {otel_nova_metric_attributes, Attrs}, State = #state{extra_metric_attrs = Extra}
) when
    is_map(Attrs)
->
    Valid =
        #{Key => Value || Key := Value <- Attrs, otel_attributes:is_valid_attribute(Key, Value)},
    {[], State#state{extra_metric_attrs = maps:merge(Extra, Valid)}};
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
    Method = safely(init, fun() -> otel_nova_http:method(cowboy_req:method(Req)) end, ~"_OTHER"),
    Attrs0 = safely(init, fun() -> request_attributes(Req, Method) end, #{
        'http.request.method' => Method
    }),
    Attrs1 = maps:merge(Attrs0, safely(init, fun() -> query_attributes(Req) end, #{})),
    {Sensitive, ResponseHeaders} = safely(
        init, fun() -> header_options() end, {?SENSITIVE_HEADERS, []}
    ),
    Attrs2 = maps:merge(
        Attrs1, safely(init, fun() -> request_header_attributes(Req, Sensitive) end, #{})
    ),
    OptIn = safely(init, fun() -> opt_in() end, []),
    Attrs = maps:merge(
        Attrs2, safely(init, fun() -> opt_in_request_attributes(Req, Attrs0, OptIn) end, #{})
    ),
    MetricAttrs = safely(init, fun() -> metric_attributes(Req, Method) end, #{
        'http.request.method' => Method
    }),
    {ParentCtx, Links} = safely(init, fun() -> parent(Req) end, {otel_ctx:new(), []}),
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
        span_ctx = SpanCtx,
        metric_attrs = MetricAttrs,
        response_headers = ResponseHeaders,
        sensitive_headers = Sensitive,
        opt_in = OptIn
    }}.

span_name(~"_OTHER") -> ~"HTTP";
span_name(Method) -> <<"HTTP ", Method/binary>>.

%% With trust_incoming_context disabled the server span is a new root, so a
%% client cannot force sampling or inject baggage, and the remote parent is
%% kept as a link.
parent(Req) ->
    Headers = maps:to_list(cowboy_req:headers(Req)),
    Remote = otel_propagator_text_map:extract_to(otel_ctx:new(), Headers),
    case trust_incoming_context(Req) of
        true ->
            {Remote, []};
        false ->
            {otel_ctx:new(), links(otel_tracer:current_span_ctx(Remote))}
    end.

trust_incoming_context(Req) ->
    case opt(trust_incoming_context, true) of
        true ->
            true;
        {Mod, Fun} when is_atom(Mod), is_atom(Fun) ->
            try
                Mod:Fun(Req) =:= true
            catch
                Class:Reason:Stacktrace ->
                    log_failure(trust_incoming_context, Class, Reason, Stacktrace),
                    false
            end;
        _ ->
            false
    end.

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
        response_header_attrs = HeaderAttrs
    }
) ->
    _ = otel_span:set_attributes(SpanCtx, maps:merge(HeaderAttrs, body_size_attributes(State))),
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
    BuiltinAttrs = maps:merge(
        MetricAttrs, maps:merge(route_attrs(Route), status_attrs(StatusCode, Tag))
    ),
    FinalAttrs = maps:merge(maps:without(?BUILTIN_METRIC_ATTRIBUTES, Extra), BuiltinAttrs),
    ?histogram_record('http.server.request.duration', Duration, FinalAttrs),
    record_if_positive('http.server.request.body.size', ReqBodyLen, FinalAttrs),
    record_if_positive('http.server.response.body.size', RespBodyLen, FinalAttrs),
    ok.

set_status_attributes(_SpanCtx, undefined, _Tag) ->
    ok;
set_status_attributes(SpanCtx, Code, Tag) when Code >= 500 ->
    _ = otel_span:set_attributes(SpanCtx, #{
        'http.response.status_code' => Code,
        'error.type' => error_type(Code, Tag)
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

route_attrs(undefined) -> #{};
route_attrs(Route) -> #{'http.route' => Route}.

status_attrs(undefined, undefined) ->
    #{};
status_attrs(undefined, Tag) ->
    #{'error.type' => Tag};
status_attrs(Code, Tag) when Code >= 500 ->
    #{'http.response.status_code' => Code, 'error.type' => error_type(Code, Tag)};
status_attrs(Code, _Tag) ->
    #{'http.response.status_code' => Code}.

error_type(Code, undefined) -> integer_to_binary(Code);
error_type(_Code, Tag) -> Tag.

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
            log_failure(Callback, Class, Reason, Stacktrace),
            Default
    end.

-doc false.
-spec log_failure(atom(), error | exit | throw, term(), [tuple()]) -> ok.
log_failure(Callback, Class, Reason, Stacktrace) ->
    ?LOG_WARNING(#{
        what => otel_nova_instrumentation_failed,
        callback => Callback,
        class => Class,
        reason => reason_tag(Reason),
        stacktrace => [strip_args(Frame) || Frame <- Stacktrace]
    }),
    ok.

reason_tag(Reason) when is_atom(Reason) -> Reason;
reason_tag(Reason) when is_tuple(Reason), is_atom(element(1, Reason)) -> element(1, Reason);
reason_tag(_Reason) -> unknown.

strip_args({Module, Function, Args, Location}) when is_list(Args) ->
    {Module, Function, length(Args), Location};
strip_args(Frame) ->
    Frame.

request_attributes(Req, Method) ->
    OriginalMethod = cowboy_req:method(Req),
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
    Attrs1 =
        case OriginalMethod of
            Method -> Attrs0;
            _ -> Attrs0#{'http.request.method_original' => OriginalMethod}
        end,
    case cowboy_req:header(~"user-agent", Req) of
        undefined -> Attrs1;
        UA -> Attrs1#{'user_agent.original' => UA}
    end.

query_attributes(Req) ->
    case {opt(url_query, true), cowboy_req:qs(Req)} of
        {true, Qs} when Qs =/= ~"" ->
            Query = otel_nova_http:redact_query(Qs, opt(url_query_redact_keys, [])),
            case otel_nova_http:is_utf8(Query) of
                true -> #{'url.query' => Query};
                false -> #{}
            end;
        _ ->
            #{}
    end.

header_options() ->
    Sensitive = [
        otel_nova_http:lowercase(Name)
     || Name <- opt(sensitive_headers, ?SENSITIVE_HEADERS)
    ],
    ResponseHeaders = [otel_nova_http:lowercase(Name) || Name <- opt(response_headers, [])],
    {Sensitive, ResponseHeaders}.

request_header_attributes(Req, Sensitive) ->
    Names = [otel_nova_http:lowercase(Name) || Name <- opt(request_headers, [])],
    Headers = [
        {Name, Value}
     || Name <- Names, Value <- [cowboy_req:header(Name, Req)], Value =/= undefined
    ],
    otel_nova_http:header_attributes(~"http.request.header.", Headers, Sensitive).

capture_response_headers(_Info, State = #state{response_headers = []}) ->
    State;
capture_response_headers({response, _Code, Headers, _Body}, State) ->
    State#state{response_header_attrs = response_header_attributes(Headers, State)};
capture_response_headers({headers, _Code, Headers}, State) ->
    State#state{response_header_attrs = response_header_attributes(Headers, State)};
capture_response_headers(_Info, State) ->
    State.

response_header_attributes(Headers, #state{response_headers = Names, sensitive_headers = Sensitive}) ->
    Captured = [{Name, Value} || Name := Value <- Headers, lists:member(Name, Names)],
    otel_nova_http:header_attributes(~"http.response.header.", Captured, Sensitive).

-define(OPT_IN_ATTRIBUTES, [
    'client.port',
    'network.local.address',
    'network.local.port',
    'network.transport',
    'http.request.body.size',
    'http.response.body.size'
]).

opt_in() ->
    [Name || Name <- opt(opt_in_attributes, []), lists:member(Name, ?OPT_IN_ATTRIBUTES)].

%% client.port is only known when client.address is the peer, not an address
%% taken from a forwarding header.
opt_in_request_attributes(_Req, _Attrs, []) ->
    #{};
opt_in_request_attributes(Req, Attrs, OptIn) ->
    ClientPort =
        case Attrs of
            #{
                'client.address' := Peer,
                'network.peer.address' := Peer,
                'network.peer.port' := Port
            } ->
                #{'client.port' => Port};
            #{} ->
                #{}
        end,
    {LocalIP, LocalPort} = cowboy_req:sock(Req),
    All = ClientPort#{
        'network.local.address' => peer_to_binary(LocalIP),
        'network.local.port' => LocalPort,
        'network.transport' => transport(Req)
    },
    maps:with(OptIn, All).

transport(#{version := 'HTTP/3'}) -> ~"udp";
transport(_Req) -> ~"tcp".

body_size_attributes(#state{opt_in = OptIn, req_body_length = Req, resp_body_length = Resp}) ->
    maps:with(OptIn, #{'http.request.body.size' => Req, 'http.response.body.size' => Resp}).

opt(Key, Default) ->
    application:get_env(opentelemetry_nova, Key, Default).

%% Metric attributes become Prometheus series, so every value is bounded:
%% the method and scheme are normalised, and the server address and port are
%% only recorded for hosts on the metric_server_hosts allowlist, with the
%% port taken from the socket rather than the Host header.
metric_attributes(Req, Method) ->
    Attrs = #{
        'http.request.method' => Method,
        'url.scheme' => otel_nova_http:scheme(cowboy_req:scheme(Req))
    },
    case application:get_env(opentelemetry_nova, metric_server_hosts, undefined) of
        Hosts when is_list(Hosts) -> maps:merge(Attrs, server_attributes(Req, Hosts));
        _ -> Attrs
    end.

server_attributes(Req, Hosts) ->
    Host = cowboy_req:host(Req),
    Address =
        case lists:member(Host, Hosts) of
            true -> Host;
            false -> ~"_OTHER"
        end,
    {_IP, Port} = cowboy_req:sock(Req),
    #{'server.address' => Address, 'server.port' => Port}.

-define(FORWARDED_HEADERS, [~"forwarded", ~"x-forwarded-for", ~"x-real-ip"]).

client_address(Req, PeerBin) ->
    case opt(trust_forwarded_for, false) of
        true ->
            otel_nova_http:client_address(Req, PeerBin, [~"x-forwarded-for", ~"forwarded"]);
        Headers when is_list(Headers) ->
            Trusted = [H || H <- Headers, lists:member(H, ?FORWARDED_HEADERS)],
            otel_nova_http:client_address(Req, PeerBin, Trusted);
        _ ->
            PeerBin
    end.

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

record_if_positive(Name, Value, Attrs) when Value > 0 ->
    ?histogram_record(Name, Value, Attrs);
record_if_positive(_, _, _) ->
    ok.
