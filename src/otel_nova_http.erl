-module(otel_nova_http).
-moduledoc false.

-export([method/1, scheme/1, span_method/1, client_address/2, is_utf8/1]).

-define(KNOWN_METHODS, [
    ~"GET", ~"HEAD", ~"POST", ~"PUT", ~"DELETE", ~"CONNECT", ~"OPTIONS", ~"TRACE", ~"PATCH"
]).

method(Method) ->
    case lists:member(Method, ?KNOWN_METHODS) of
        true -> Method;
        false -> ~"_OTHER"
    end.

scheme(~"http") -> ~"http";
scheme(~"https") -> ~"https";
scheme(_) -> ~"_OTHER".

span_method(~"_OTHER") -> ~"HTTP";
span_method(Method) -> Method.

client_address(Req, Peer) ->
    case cowboy_req:header(~"x-forwarded-for", Req) of
        undefined ->
            case cowboy_req:header(~"forwarded", Req) of
                undefined -> Peer;
                Forwarded -> valid_or(forwarded_for(Forwarded), Peer)
            end;
        XFF ->
            [First | _] = binary:split(XFF, ~","),
            valid_or(trim(First), Peer)
    end.

is_utf8(Bin) ->
    unicode:characters_to_binary(Bin) =:= Bin.

forwarded_for(Forwarded) ->
    case re:run(Forwarded, ~"for=\"?([^;,\"]+)\"?", [{capture, [1], binary}, caseless]) of
        {match, [Node]} -> strip_port(trim(Node));
        nomatch -> undefined
    end.

strip_port(<<"[", Rest/binary>>) ->
    hd(binary:split(Rest, ~"]"));
strip_port(Node) ->
    case binary:split(Node, ~":", [global]) of
        [Addr, _Port] -> Addr;
        _ -> Node
    end.

valid_or(undefined, Default) ->
    Default;
valid_or(Addr, Default) ->
    case inet:parse_strict_address(binary_to_list(Addr)) of
        {ok, _} -> Addr;
        {error, _} -> Default
    end.

trim(Bin) ->
    trim_trailing(trim_leading(Bin)).

trim_leading(<<C, Rest/binary>>) when C =:= $\s; C =:= $\t -> trim_leading(Rest);
trim_leading(Bin) -> Bin.

trim_trailing(<<>>) ->
    <<>>;
trim_trailing(Bin) ->
    Last = binary:last(Bin),
    case Last =:= $\s orelse Last =:= $\t of
        true -> trim_trailing(binary:part(Bin, 0, byte_size(Bin) - 1));
        false -> Bin
    end.
