-module(otel_nova_test_controller).

-export([ok/1, crash/1, crash_tuple/1, throw_status/1, labelled/1]).
-export([crash_security/1, crash_plugin/4]).
-export([span_name/2, crashing_span_name/2, invalid_span_name/2]).
-export([trust_internal/1, crashing_trust/1]).

ok(_Req) ->
    {status, 200}.

crash(_Req) ->
    error(badarg).

crash_tuple(Req) ->
    error({badmatch, maps:get(path, Req)}).

throw_status(_Req) ->
    throw({503, unavailable}).

labelled(Req) ->
    ok = opentelemetry_nova:add_metric_attributes(Req, #{
        tenant => ~"acme", 'http.route' => ~"/overridden"
    }),
    {status, 200}.

crash_security(_Req) ->
    error(security_boom).

crash_plugin(_Req, _Env, _Opts, _State) ->
    error(plugin_boom).

span_name(Method, undefined) ->
    <<Method/binary, " unrouted">>;
span_name(Method, Route) ->
    <<"custom ", Method/binary, " ", Route/binary>>.

crashing_span_name(_Method, _Route) ->
    error(formatter_boom).

invalid_span_name(_Method, _Route) ->
    "not a binary".

trust_internal(Req) ->
    cowboy_req:header(~"x-internal", Req) =:= ~"yes".

crashing_trust(_Req) ->
    error(trust_boom).
