-module(otel_nova_http).
-moduledoc false.

-export([method/1, scheme/1, span_method/1, client_address/3, is_utf8/1]).
-export([redact_query/2, lowercase/1, header_attributes/3]).

-define(KNOWN_METHODS, [
    ~"GET", ~"HEAD", ~"POST", ~"PUT", ~"DELETE", ~"CONNECT", ~"OPTIONS", ~"TRACE", ~"PATCH"
]).

-define(REDACTED_QUERY_KEYS, [
    ~"awsaccesskeyid",
    ~"signature",
    ~"sig",
    ~"x-goog-signature",
    ~"x-amz-signature",
    ~"x-amz-credential",
    ~"x-amz-security-token",
    ~"access_token",
    ~"id_token",
    ~"refresh_token",
    ~"token",
    ~"api_key",
    ~"apikey",
    ~"key",
    ~"password",
    ~"secret",
    ~"code",
    ~"state"
]).

-spec method(binary()) -> binary().
method(Method) ->
    case lists:member(Method, ?KNOWN_METHODS) of
        true -> Method;
        false -> ~"_OTHER"
    end.

-spec scheme(binary()) -> binary().
scheme(~"http") -> ~"http";
scheme(~"https") -> ~"https";
scheme(_) -> ~"_OTHER".

-spec span_method(binary()) -> binary().
span_method(~"_OTHER") -> ~"HTTP";
span_method(Method) -> Method.

%% The first of Headers that the request carries decides; when its address is
%% not a valid IP the peer address is used, not a later header.
-spec client_address(cowboy_req:req(), binary(), [binary()]) -> binary().
client_address(_Req, Peer, []) ->
    Peer;
client_address(Req, Peer, [Header | Headers]) ->
    case cowboy_req:header(Header, Req) of
        undefined -> client_address(Req, Peer, Headers);
        Value -> valid_or(header_address(Header, Value), Peer)
    end.

header_address(~"x-forwarded-for", Value) ->
    [First | _] = binary:split(Value, ~","),
    trim(First);
header_address(~"forwarded", Value) ->
    forwarded_for(Value);
header_address(~"x-real-ip", Value) ->
    strip_port(trim(Value)).

-spec is_utf8(binary()) -> boolean().
is_utf8(Bin) ->
    unicode:characters_to_binary(Bin) =:= Bin.

%% Only the values of matching keys are replaced; everything else, including
%% the encoding of the other pairs, is kept as sent.
-spec redact_query(binary(), [binary()]) -> binary().
redact_query(Qs, ExtraKeys) ->
    Keys = ?REDACTED_QUERY_KEYS ++ [lowercase(Key) || Key <- ExtraKeys],
    Pairs = binary:split(Qs, ~"&", [global]),
    iolist_to_binary(lists:join(~"&", [redact_pair(Pair, Keys) || Pair <- Pairs])).

redact_pair(Pair, Keys) ->
    case binary:split(Pair, ~"=") of
        [Key, _Value] ->
            case lists:member(query_key(Key), Keys) of
                true -> <<Key/binary, "=REDACTED">>;
                false -> Pair
            end;
        [_Key] ->
            Pair
    end.

query_key(Key) ->
    Plain = binary:replace(Key, ~"+", ~" ", [global]),
    case uri_string:percent_decode(Plain) of
        Decoded when is_binary(Decoded) -> lowercase(Decoded);
        {error, _, _} -> lowercase(Plain)
    end.

%% Semconv records a header as a list of values. Cowboy joins repeated request
%% headers into one value and passes the cookies of a response as a list.
-spec header_attributes(binary(), [{binary(), iodata() | [iodata()]}], [binary()]) ->
    #{binary() => [binary()]}.
header_attributes(Prefix, Headers, Sensitive) ->
    maps:from_list([
        {<<Prefix/binary, Name/binary>>, Values}
     || {Name, Value} <- Headers,
        Values <- [header_values(Name, Value, Sensitive)],
        Values =/= []
    ]).

header_values(Name, Value, Sensitive) ->
    case lists:member(Name, Sensitive) of
        true -> [~"REDACTED"];
        false -> [V || V <- raw_header_values(Name, Value), is_utf8(V)]
    end.

raw_header_values(~"set-cookie", Cookies) when is_list(Cookies) ->
    [iolist_to_binary(Cookie) || Cookie <- Cookies];
raw_header_values(_Name, Value) ->
    [iolist_to_binary(Value)].

-spec lowercase(binary()) -> binary().
lowercase(Bin) ->
    <<<<(ascii_lower(C))>> || <<C>> <= Bin>>.

ascii_lower(C) when C >= $A, C =< $Z -> C + 32;
ascii_lower(C) -> C.

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
