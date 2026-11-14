-module(otel_nova_test_sse).
-behaviour(cowboy_handler).

-export([init/2]).

-define(HOLD_MS, 400).

init(Req0 = #{path := Path}, State) ->
    ContentType =
        case Path of
            ~"/nova/sse/marked" ->
                ok = opentelemetry_nova:set_stream_duration(Req0, first_byte),
                ~"application/octet-stream";
            _ ->
                ~"Text/Event-Stream; charset=utf-8"
        end,
    Req = cowboy_req:stream_reply(200, #{~"content-type" => ContentType}, Req0),
    ok = cowboy_req:stream_body(~"data: first\n\n", nofin, Req),
    receive
    after ?HOLD_MS -> ok
    end,
    ok = cowboy_req:stream_body(~"data: last\n\n", fin, Req),
    {ok, Req, State}.
