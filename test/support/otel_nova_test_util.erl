-module(otel_nova_test_util).

-export([stream_req/2, span_attrs/1, receive_span/1]).

-include_lib("opentelemetry/include/otel_span.hrl").

-spec stream_req(binary(), binary()) -> cowboy_req:req().
stream_req(Method, Path) ->
    #{
        method => Method,
        version => 'HTTP/1.1',
        scheme => ~"http",
        host => ~"localhost",
        port => 8080,
        path => Path,
        qs => ~"",
        headers => #{~"host" => ~"localhost:8080"},
        peer => {{127, 0, 0, 1}, 12345},
        sock => {{127, 0, 0, 1}, 8080},
        cert => undefined,
        ref => otel_nova_test,
        pid => self(),
        streamid => 1,
        has_body => false,
        body_length => 0
    }.

-spec span_attrs(#span{}) -> #{atom() | binary() => term()}.
span_attrs(#span{attributes = undefined}) ->
    #{};
span_attrs(#span{attributes = '_'}) ->
    #{};
span_attrs(#span{attributes = Attributes}) ->
    otel_attributes:map(Attributes).

%% Spans of earlier requests can still arrive, so wait for the one with Path.
-spec receive_span(binary()) -> #span{}.
receive_span(Path) ->
    receive
        {span, Span} ->
            case span_attrs(Span) of
                #{'url.path' := Path} -> Span;
                _ -> receive_span(Path)
            end
    after 5000 ->
        error({timeout_waiting_for_span, Path})
    end.
