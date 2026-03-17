-module(otel_nova_sampler).
-behavior(otel_sampler).

-export([setup/1, description/1, should_sample/7]).

-include_lib("opentelemetry_api/include/opentelemetry.hrl").
-include_lib("opentelemetry/include/otel_sampler.hrl").

setup(ExcludeList) when is_list(ExcludeList) ->
    #{exclude => ExcludeList}.

description(#{exclude := List}) ->
    iolist_to_binary(io_lib:format("Exclude server spans: ~p", [List])).

should_sample(_Ctx, _TraceId, _Links, Name, Kind, _Attrs, #{exclude := List}) ->
    case Kind =:= ?SPAN_KIND_SERVER andalso lists:member(Name, List) of
        true -> {?DROP, [], []};
        false -> {?RECORD_AND_SAMPLE, [], []}
    end.
