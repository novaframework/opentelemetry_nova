-module(otel_nova_sampler).
-moduledoc """
Sampler that drops server spans for excluded requests, such as health checks
and Prometheus scrapes.

Configure it with a list of `"METHOD /path"` binaries:

```erlang
{opentelemetry, [
    {sampler, {parent_based, #{root => {otel_nova_sampler, [~"GET /metrics", ~"GET /healthz"]}}}}
]}
```

The decision is made when `otel_nova_stream_h` starts the server span, before
Nova has routed the request, so the route template is not known yet. An entry
is compared with the request method and the raw request path (the
`http.request.method` and `url.path` attributes, without the query string),
not with a route template: `~"GET /users/:id"` does not match `/users/42`.
An entry that equals the span name also matches, for server spans started
elsewhere with a final name.

Only server spans are dropped. Wrapped in `parent_based`, it decides for root
spans only; a request with a sampled `traceparent` follows its parent.

To sample the remaining spans with another sampler, pass a map with the
exclusions and a sampler spec, which defaults to `always_on`:

```erlang
{otel_nova_sampler, #{
    exclude => [~"GET /metrics"],
    sampler => {trace_id_ratio_based, 0.1}
}}
```
""".
-behavior(otel_sampler).

-export([setup/1, description/1, should_sample/7]).

-include_lib("opentelemetry_api/include/opentelemetry.hrl").
-include_lib("opentelemetry/include/otel_sampler.hrl").

setup(ExcludeList) when is_list(ExcludeList) ->
    setup(#{exclude => ExcludeList});
setup(Opts) when is_map(Opts) ->
    #{
        exclude => maps:get(exclude, Opts, []),
        sampler => otel_sampler:new(maps:get(sampler, Opts, always_on))
    }.

description(#{exclude := List, sampler := Sampler}) ->
    iolist_to_binary(
        io_lib:format("Exclude server spans: ~p, else ~ts", [
            List, otel_sampler:description(Sampler)
        ])
    ).

should_sample(Ctx, TraceId, Links, Name, Kind, Attrs, #{exclude := List, sampler := Sampler}) ->
    case Kind =:= ?SPAN_KIND_SERVER andalso excluded(Name, Attrs, List) of
        true -> {?DROP, [], []};
        false -> otel_sampler:should_sample(Sampler, Ctx, TraceId, Links, Name, Kind, Attrs)
    end.

excluded(Name, Attrs, List) ->
    lists:member(Name, List) orelse lists:member(request_name(Attrs), List).

request_name(#{'http.request.method' := Method, 'url.path' := Path}) when
    is_binary(Method), is_binary(Path)
->
    <<Method/binary, " ", Path/binary>>;
request_name(_Attrs) ->
    undefined.
