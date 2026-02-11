-module(otel_nova_prom_handler).

-export([init/2]).

init(Req0, State) ->
    Body = otel_nova_prom_exporter:get_metrics(),
    Req = cowboy_req:reply(200, #{
        <<"content-type">> => <<"text/plain; version=0.0.4; charset=utf-8">>
    }, Body, Req0),
    {ok, Req, State}.
