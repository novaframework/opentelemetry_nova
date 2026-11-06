-module(test_metric_exporter).

-export([init/1, export/3, export/4, shutdown/1]).

init(_) ->
    {ok, []}.

export(Metrics, _Resource, _State) ->
    case persistent_term:get(test_metric_pid, undefined) of
        undefined ->
            ok;
        Pid ->
            [Pid ! {otel_metric, M} || M <- Metrics]
    end,
    ok.

export(metrics, Metrics, Resource, State) ->
    export(Metrics, Resource, State).

shutdown(_) ->
    ok.
