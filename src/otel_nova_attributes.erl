-module(otel_nova_attributes).
-moduledoc false.

-export([options/0, span/3, metric/3, response_headers/2, body_sizes/3, final_metric/5]).

-export_type([options/0]).

-define(SENSITIVE_HEADERS, [
    ~"authorization", ~"proxy-authorization", ~"cookie", ~"set-cookie", ~"x-api-key"
]).

-define(FORWARDED_HEADERS, [~"forwarded", ~"x-forwarded-for", ~"x-real-ip"]).

-define(OPT_IN_ATTRIBUTES, [
    'client.port',
    'network.local.address',
    'network.local.port',
    'network.transport',
    'http.request.body.size',
    'http.response.body.size'
]).

-define(BUILTIN_METRIC_ATTRIBUTES, [
    'http.request.method',
    'url.scheme',
    'http.route',
    'http.response.status_code',
    'error.type',
    'server.address',
    'server.port'
]).

-type options() :: #{
    trust_forwarded_for := [binary()],
    metric_server_hosts := [binary()] | undefined,
    trust_incoming_context := boolean() | {module(), atom()},
    url_query := boolean(),
    url_query_redact_keys := [binary()],
    request_headers := [binary()],
    sensitive_headers := [binary()],
    response_headers := [binary()],
    opt_in_attributes := [atom()]
}.

%% Read once per request; every value is normalised so that a malformed
%% setting falls back to its default instead of failing the request.
-spec options() -> options().
options() ->
    Env = maps:from_list(application:get_all_env(opentelemetry_nova)),
    #{
        trust_forwarded_for => forwarded_headers(maps:get(trust_forwarded_for, Env, false)),
        metric_server_hosts =>
            case maps:get(metric_server_hosts, Env, undefined) of
                Hosts when is_list(Hosts) -> [Host || Host <- Hosts, is_binary(Host)];
                _ -> undefined
            end,
        trust_incoming_context =>
            case maps:get(trust_incoming_context, Env, true) of
                true -> true;
                {Mod, Fun} when is_atom(Mod), is_atom(Fun) -> {Mod, Fun};
                _ -> false
            end,
        url_query => maps:get(url_query, Env, true) =:= true,
        url_query_redact_keys => names(maps:get(url_query_redact_keys, Env, [])),
        request_headers => names(maps:get(request_headers, Env, [])),
        sensitive_headers =>
            case maps:get(sensitive_headers, Env, ?SENSITIVE_HEADERS) of
                Sensitive when is_list(Sensitive) -> names(Sensitive);
                _ -> ?SENSITIVE_HEADERS
            end,
        response_headers => names(maps:get(response_headers, Env, [])),
        opt_in_attributes =>
            case maps:get(opt_in_attributes, Env, []) of
                OptIn when is_list(OptIn) -> [A || A <- OptIn, lists:member(A, ?OPT_IN_ATTRIBUTES)];
                _ -> []
            end
    }.

forwarded_headers(true) ->
    [~"x-forwarded-for", ~"forwarded"];
forwarded_headers(Headers) when is_list(Headers) ->
    [H || H <- Headers, lists:member(H, ?FORWARDED_HEADERS)];
forwarded_headers(_) ->
    [].

names(Names) when is_list(Names) ->
    [otel_nova_http:lowercase(Name) || Name <- Names, is_binary(Name)];
names(_) ->
    [].

%% Each group of attributes is built on its own, so a failure in one leaves
%% the others on the span.
-spec span(cowboy_req:req(), binary(), options()) -> opentelemetry:attributes_map().
span(Req, Method, Opts) ->
    Base = safely(fun() -> base(Req, Method, Opts) end, #{'http.request.method' => Method}),
    Query = safely(fun() -> query(Req, Opts) end, #{}),
    Headers = safely(fun() -> request_headers(Req, Opts) end, #{}),
    OptIn = safely(fun() -> opt_in(Req, Base, Opts) end, #{}),
    maps:merge(Base, maps:merge(Query, maps:merge(Headers, OptIn))).

base(Req, Method, Opts) ->
    OriginalMethod = cowboy_req:method(Req),
    {PeerAddr, PeerPort} = cowboy_req:peer(Req),
    PeerBin = peer_to_binary(PeerAddr),
    Attrs0 = #{
        'http.request.method' => Method,
        'url.path' => cowboy_req:path(Req),
        'url.scheme' => cowboy_req:scheme(Req),
        'server.address' => cowboy_req:host(Req),
        'server.port' => cowboy_req:port(Req),
        'network.peer.address' => PeerBin,
        'network.peer.port' => PeerPort,
        'network.protocol.version' => protocol_version(Req),
        'client.address' => otel_nova_http:client_address(
            Req, PeerBin, maps:get(trust_forwarded_for, Opts)
        )
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

query(Req, #{url_query := true, url_query_redact_keys := Keys}) ->
    case cowboy_req:qs(Req) of
        ~"" ->
            #{};
        Qs ->
            Query = otel_nova_http:redact_query(Qs, Keys),
            case otel_nova_http:is_utf8(Query) of
                true -> #{'url.query' => Query};
                false -> #{}
            end
    end;
query(_Req, _Opts) ->
    #{}.

request_headers(Req, #{request_headers := Names, sensitive_headers := Sensitive}) ->
    Headers = [
        {Name, Value}
     || Name <- Names, Value <- [cowboy_req:header(Name, Req)], Value =/= undefined
    ],
    otel_nova_http:header_attributes(~"http.request.header.", Headers, Sensitive).

-spec response_headers(cowboy:http_headers(), options()) -> opentelemetry:attributes_map().
response_headers(Headers, #{response_headers := Names, sensitive_headers := Sensitive}) ->
    Captured = [{Name, Value} || Name := Value <- Headers, lists:member(Name, Names)],
    otel_nova_http:header_attributes(~"http.response.header.", Captured, Sensitive).

%% client.port is only known when client.address is the peer, not an address
%% taken from a forwarding header.
opt_in(_Req, _Attrs, #{opt_in_attributes := []}) ->
    #{};
opt_in(Req, Attrs, #{opt_in_attributes := OptIn}) ->
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

-spec body_sizes(options(), non_neg_integer(), non_neg_integer()) ->
    opentelemetry:attributes_map().
body_sizes(#{opt_in_attributes := OptIn}, ReqLength, RespLength) ->
    maps:with(OptIn, #{
        'http.request.body.size' => ReqLength, 'http.response.body.size' => RespLength
    }).

%% Metric attributes become Prometheus series, so every value is bounded:
%% the method and scheme are normalised, and the server address and port are
%% only recorded for hosts on the metric_server_hosts allowlist, with the
%% port taken from the socket rather than the Host header.
-spec metric(cowboy_req:req(), binary(), options()) -> opentelemetry:attributes_map().
metric(Req, Method, #{metric_server_hosts := Hosts}) ->
    Attrs = #{
        'http.request.method' => Method,
        'url.scheme' => otel_nova_http:scheme(cowboy_req:scheme(Req))
    },
    case Hosts of
        undefined -> Attrs;
        _ -> maps:merge(Attrs, server(Req, Hosts))
    end.

server(Req, Hosts) ->
    Host = cowboy_req:host(Req),
    Address =
        case lists:member(Host, Hosts) of
            true -> Host;
            false -> ~"_OTHER"
        end,
    {_IP, Port} = cowboy_req:sock(Req),
    #{'server.address' => Address, 'server.port' => Port}.

-spec final_metric(map(), binary() | undefined, integer() | undefined, binary() | undefined, map()) ->
    map().
final_metric(MetricAttrs, Route, StatusCode, ErrorType, Extra) ->
    Builtin = maps:merge(MetricAttrs, maps:merge(route(Route), status(StatusCode, ErrorType))),
    maps:merge(maps:without(?BUILTIN_METRIC_ATTRIBUTES, Extra), Builtin).

route(undefined) -> #{};
route(Route) -> #{'http.route' => Route}.

status(undefined, undefined) ->
    #{};
status(undefined, Tag) ->
    #{'error.type' => Tag};
status(Code, Tag) when Code >= 500 ->
    #{'http.response.status_code' => Code, 'error.type' => otel_nova_http:error_type(Code, Tag)};
status(Code, _Tag) ->
    #{'http.response.status_code' => Code}.

peer_to_binary(Addr) ->
    case inet:ntoa(Addr) of
        {error, _} -> ~"unknown";
        String -> list_to_binary(String)
    end.

transport(#{version := 'HTTP/3'}) -> ~"udp";
transport(_Req) -> ~"tcp".

protocol_version(#{version := 'HTTP/1.0'}) -> ~"1.0";
protocol_version(#{version := 'HTTP/1.1'}) -> ~"1.1";
protocol_version(#{version := 'HTTP/2'}) -> ~"2";
protocol_version(#{version := 'HTTP/3'}) -> ~"3";
protocol_version(_) -> ~"1.1".

safely(Fun, Default) ->
    try
        Fun()
    catch
        Class:Reason:Stacktrace ->
            otel_nova_http:log_failure(init, Class, Reason, Stacktrace),
            Default
    end.
