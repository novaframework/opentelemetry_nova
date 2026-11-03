-module(otel_nova_test_handler).
-behaviour(cowboy_middleware).

-export([execute/2]).

-include_lib("opentelemetry_api/include/otel_tracer.hrl").

execute(Req = #{path := ~"/child"}, _Env) ->
    ?with_span(~"child", #{}, fun(_SpanCtx) -> ok end),
    {stop, cowboy_req:reply(200, #{}, ~"ok", Req)};
execute(Req = #{path := ~"/static"}, #{static_file := {Path, Size}}) ->
    {stop, cowboy_req:reply(200, #{}, {sendfile, 0, Size, Path}, Req)};
execute(Req = #{path := <<"/assets/", _/binary>>}, _Env) ->
    {stop, cowboy_req:reply(404, #{}, ~"not found", Req)}.
