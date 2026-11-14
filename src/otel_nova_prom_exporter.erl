-module(otel_nova_prom_exporter).
-moduledoc """
Metric exporter that renders OpenTelemetry metrics in the Prometheus text format.

Configure it as the exporter of an `otel_metric_reader` and serve the result
with `otel_nova_prom_server` (or `otel_nova_prom_handler` on a listener of your
own). Delta datapoints are accumulated into cumulative values, cumulative
datapoints replace the stored value, so the output is correct for either
reader temporality.

Each metric keeps at most 2000 series. Datapoints for further attribute sets
are added to one overflow series labelled `otel_metric_overflow="true"`, so
an unexpected source of label values cannot grow the table without bound.
Label values that are not valid UTF-8 are rendered as `_INVALID_UTF8`.

The table is written only by the process that called `init/1`, which is the
metric reader.

Implements the exporter API of both opentelemetry_experimental 0.6
(`export/3`, `shutdown/1`) and 0.5 (`export/4`, `shutdown/1`).
""".

-export([init/1, export/3, export/4, force_flush/0, shutdown/0, shutdown/1]).
-export([get_metrics/0]).

-include_lib("kernel/include/logger.hrl").
-include_lib("opentelemetry_api_experimental/include/otel_metrics.hrl").
-include_lib("opentelemetry_experimental/include/otel_metrics.hrl").

-define(TABLE, otel_nova_prom_metrics).
-define(MAX_SERIES, 2000).
-define(OVERFLOW_ATTRS, [{'otel.metric.overflow', true}]).

-type label_attributes() :: ordsets:ordset(opentelemetry:attribute()).
-type series_key() :: {dp, binary(), label_attributes()}.
-type bounds() :: match_spec([float()]) | undefined.
-type series() ::
    {hist, number(), number() | undefined, list(), bounds()}
    | {counter | gauge, number()}.

-spec init(term()) -> {ok, map()} | ignore.
init(_Config) ->
    case ets:info(?TABLE, owner) of
        undefined ->
            _ = ets:new(?TABLE, [named_table, protected, set, {read_concurrency, true}]),
            {ok, #{}};
        Owner when Owner =:= self() ->
            {ok, #{}};
        Owner ->
            ?LOG_WARNING(#{
                what => otel_nova_prom_exporter_already_running,
                owner => Owner,
                reason => ~"another metric reader already exports to otel_nova_prom_exporter"
            }),
            ignore
    end.

-spec export([#metric{}], term(), term()) -> ok.
export(Metrics, _Resource, _State) ->
    _ = [accumulate_metric(Metric) || Metric <- Metrics],
    Output = render_all(),
    ets:insert(?TABLE, {metrics_output, Output}),
    ok.

-spec export(metrics, [#metric{}], term(), term()) -> ok.
export(metrics, Metrics, Resource, State) ->
    export(Metrics, Resource, State).

-spec force_flush() -> ok.
force_flush() ->
    ok.

-spec shutdown(term()) -> ok.
shutdown(_State) ->
    shutdown().

-spec shutdown() -> ok.
shutdown() ->
    case ets:info(?TABLE) of
        undefined -> ok;
        _ -> ets:delete(?TABLE)
    end,
    ok.

-spec get_metrics() -> binary().
get_metrics() ->
    case ets:info(?TABLE) of
        undefined ->
            <<>>;
        _ ->
            case ets:lookup(?TABLE, metrics_output) of
                [{metrics_output, Output}] when is_binary(Output) -> Output;
                [] -> <<>>
            end
    end.

%%--------------------------------------------------------------------
%% Internal - accumulation into cumulative values
%%--------------------------------------------------------------------

-spec accumulate_metric(#metric{}) -> ok.
accumulate_metric(#metric{name = Name, description = Desc, unit = Unit, data = Data}) ->
    PromName = prom_metric_name(Name, Unit, Data),
    Type = prom_type(Data),
    ets:insert(?TABLE, {metric_meta(PromName), Desc, Type}),
    accumulate_data(PromName, Data).

-spec accumulate_data(binary(), #sum{} | #gauge{} | #histogram{}) -> ok.
accumulate_data(PromName, #histogram{datapoints = Datapoints, aggregation_temporality = T}) ->
    reset_overflow(PromName, T),
    _ = [accumulate_histogram(PromName, T, DP) || DP <- Datapoints],
    ok;
accumulate_data(PromName, #sum{
    datapoints = Datapoints, is_monotonic = Monotonic, aggregation_temporality = T
}) ->
    Kind =
        case Monotonic of
            true -> counter;
            false -> gauge
        end,
    reset_overflow(PromName, T),
    _ = [accumulate_sum(PromName, Kind, T, DP) || DP <- Datapoints],
    ok;
accumulate_data(PromName, #gauge{datapoints = Datapoints}) ->
    _ = [store_value(PromName, gauge, DP) || DP <- Datapoints],
    ok.

-spec accumulate_histogram(binary(), otel_instrument:temporality(), #histogram_datapoint{}) -> true.
accumulate_histogram(PromName, T, #histogram_datapoint{
    attributes = Attrs,
    count = Count,
    sum = Sum,
    bucket_counts = BucketCounts,
    explicit_bounds = Bounds
}) ->
    Key = dp_key(PromName, Attrs),
    case adds(T, Key) of
        true -> add_histogram(Key, Count, Sum, BucketCounts, Bounds);
        false -> ets:insert(?TABLE, {Key, {hist, Count, Sum, BucketCounts, Bounds}})
    end.

-spec add_histogram(series_key(), number(), number() | undefined, list(), bounds()) -> true.
add_histogram(Key, Count, Sum, BucketCounts, Bounds) ->
    case lookup(Key) of
        [{Key, {hist, PrevCount, PrevSum, PrevBuckets, B}}] ->
            NewBuckets = add_lists(PrevBuckets, BucketCounts),
            ets:insert(
                ?TABLE, {Key, {hist, PrevCount + Count, add_num(PrevSum, Sum), NewBuckets, B}}
            );
        [] ->
            ets:insert(?TABLE, {Key, {hist, Count, Sum, BucketCounts, Bounds}})
    end.

-spec accumulate_sum(binary(), counter | gauge, otel_instrument:temporality(), #datapoint{}) ->
    true.
accumulate_sum(PromName, Kind, T, #datapoint{attributes = Attrs, value = Value}) ->
    Key = dp_key(PromName, Attrs),
    case adds(T, Key) of
        true -> add_value(Key, Kind, Value);
        false -> ets:insert(?TABLE, {Key, {Kind, Value}})
    end.

-spec add_value(series_key(), counter | gauge, number()) -> true.
add_value(Key, Kind, Value) ->
    case lookup(Key) of
        [{Key, {Kind, PrevValue}}] ->
            ets:insert(?TABLE, {Key, {Kind, add_num(PrevValue, Value)}});
        _ ->
            ets:insert(?TABLE, {Key, {Kind, Value}})
    end.

-spec store_value(binary(), gauge, #datapoint{}) -> true.
store_value(PromName, Kind, #datapoint{attributes = Attrs, value = Value}) ->
    ets:insert(?TABLE, {dp_key(PromName, Attrs), {Kind, Value}}).

%% Delta datapoints always add up. Cumulative datapoints replace their series,
%% except in the overflow series, which is the sum of every cumulative
%% datapoint that did not fit and is reset before each metric is accumulated.
-spec adds(otel_instrument:temporality(), series_key()) -> boolean().
adds(?TEMPORALITY_DELTA, _Key) -> true;
adds(_Cumulative, {dp, _PromName, ?OVERFLOW_ATTRS}) -> true;
adds(_Cumulative, _Key) -> false.

-spec reset_overflow(binary(), otel_instrument:temporality()) -> ok | true.
reset_overflow(_PromName, ?TEMPORALITY_DELTA) ->
    ok;
reset_overflow(PromName, _Cumulative) ->
    ets:delete(?TABLE, {dp, PromName, ?OVERFLOW_ATTRS}).

-spec dp_key(binary(), opentelemetry:attributes_map()) -> series_key().
dp_key(PromName, Attrs) ->
    case normalize_attrs(Attrs) of
        ?OVERFLOW_ATTRS -> {dp, PromName, ?OVERFLOW_ATTRS};
        AttrList -> series_key(PromName, {dp, PromName, AttrList})
    end.

-spec series_key(binary(), series_key()) -> series_key().
series_key(PromName, Key) ->
    CountKey = {series, PromName},
    case ets:member(?TABLE, Key) orelse ets:lookup(?TABLE, CountKey) of
        true ->
            Key;
        [{CountKey, Count}] when is_integer(Count), Count >= ?MAX_SERIES ->
            {dp, PromName, ?OVERFLOW_ATTRS};
        _ ->
            _ = ets:update_counter(?TABLE, CountKey, 1, {CountKey, 0}),
            Key
    end.

-spec add_num(number() | undefined, number() | undefined) -> number() | undefined.
add_num(A, B) when is_number(A), is_number(B) -> A + B;
add_num(undefined, B) -> B;
add_num(A, undefined) -> A;
add_num(_, _) -> 0.

-spec add_lists(list(), list()) -> [number()].
add_lists([X | A], [Y | B]) when is_number(X), is_number(Y) ->
    [X + Y | add_lists(A, B)];
add_lists([], []) ->
    [].

-spec normalize_attrs(opentelemetry:attributes_map()) -> label_attributes().
normalize_attrs(Attrs) when is_map(Attrs) ->
    ordsets:from_list(maps:to_list(Attrs)).

-spec metric_meta(binary()) -> {meta, binary()}.
metric_meta(PromName) -> {meta, PromName}.

%% ETS returns rows untyped. The table is protected and written only by this
%% module, so a caller matches the row it stored and passes the parts on to
%% typed functions.
-spec lookup(series_key()) -> [dynamic()].
lookup(Key) ->
    ets:lookup(?TABLE, Key).

-spec sorted_match(ets:match_pattern()) -> [dynamic()].
sorted_match(Pattern) ->
    lists:sort(ets:match(?TABLE, Pattern)).

%%--------------------------------------------------------------------
%% Internal - render accumulated state
%%--------------------------------------------------------------------

-spec render_all() -> binary().
render_all() ->
    Metas = sorted_match({{meta, '$1'}, '$2', '$3'}),
    iolist_to_binary([render_metric(PromName, Desc, Type) || [PromName, Desc, Type] <- Metas]).

-spec render_metric(binary(), otel_instrument:description() | undefined, binary()) -> [iodata()].
render_metric(PromName, Desc, Type) ->
    Header = [
        <<"# HELP ">>,
        PromName,
        <<" ">>,
        to_bin(Desc),
        <<"\n">>,
        <<"# TYPE ">>,
        PromName,
        <<" ">>,
        Type,
        <<"\n">>
    ],
    DPs = sorted_match({{dp, PromName, '$1'}, '$2'}),
    Body = [
        render_datapoint(PromName, maps:from_list(AttrList), Data)
     || [AttrList, Data] <- DPs
    ],
    [Header | Body].

-spec render_datapoint(
    binary(), #{opentelemetry:attribute_key() => opentelemetry:attribute_value()}, series()
) -> [iodata()].
render_datapoint(Name, Attrs, {hist, Count, Sum, BucketCounts, Bounds}) ->
    Labels = format_labels(Attrs),
    BucketLines = format_buckets(Name, Labels, BucketCounts, Bounds),
    SumLine = [Name, <<"_sum">>, Labels, <<" ">>, format_number(Sum), <<"\n">>],
    CountLine = [Name, <<"_count">>, Labels, <<" ">>, format_number(Count), <<"\n">>],
    BucketLines ++ [SumLine, CountLine];
render_datapoint(Name, Attrs, {Kind, Value}) when Kind =:= counter; Kind =:= gauge ->
    Labels = format_labels(Attrs),
    [[Name, Labels, <<" ">>, format_number(Value), <<"\n">>]].

-spec format_buckets(binary(), iodata(), list(), bounds()) -> [iodata()].
format_buckets(Name, Labels, BucketCounts, Bounds) ->
    format_buckets(Name, Labels, BucketCounts, Bounds, 0, []).

-spec format_buckets(binary(), iodata(), list(), bounds(), number(), [iodata()]) -> [iodata()].
format_buckets(Name, Labels, [Count | RestCounts], [Bound | RestBounds], Cumulative, Acc) when
    is_number(Count)
->
    CumCount = Cumulative + Count,
    Le = format_number(Bound),
    Line = [
        Name, <<"_bucket">>, insert_le_label(Labels, Le), <<" ">>, format_number(CumCount), <<"\n">>
    ],
    format_buckets(Name, Labels, RestCounts, RestBounds, CumCount, Acc ++ [Line]);
format_buckets(Name, Labels, [Count | _], [], Cumulative, Acc) when is_number(Count) ->
    CumCount = Cumulative + Count,
    Line = [
        Name,
        <<"_bucket">>,
        insert_le_label(Labels, <<"+Inf">>),
        <<" ">>,
        format_number(CumCount),
        <<"\n">>
    ],
    Acc ++ [Line];
format_buckets(_Name, _Labels, [], _, _Cumulative, Acc) ->
    Acc.

%%--------------------------------------------------------------------
%% Internal - label formatting
%%--------------------------------------------------------------------

format_labels(Attrs) when map_size(Attrs) =:= 0 ->
    <<>>;
format_labels(Attrs) ->
    Pairs = lists:sort(
        maps:fold(
            fun(K, V, Acc) ->
                [{prom_label_name(K), prom_label_value(V)} | Acc]
            end,
            [],
            Attrs
        )
    ),
    Inner = lists:join(<<",">>, [[LK, <<"=\"">>, LV, <<"\"">>] || {LK, LV} <- Pairs]),
    [<<"{">>, Inner, <<"}">>].

insert_le_label(<<>>, Le) ->
    [<<"{le=\"">>, Le, <<"\"}">>];
insert_le_label([<<"{">>, Inner, <<"}">>], Le) ->
    [<<"{">>, Inner, <<",le=\"">>, Le, <<"\"}">>];
insert_le_label(_, Le) ->
    [<<"{le=\"">>, Le, <<"\"}">>].

%%--------------------------------------------------------------------
%% Internal - name conversion
%%--------------------------------------------------------------------

-spec prom_metric_name(
    otel_view:name(), otel_instrument:unit() | undefined, #sum{} | #gauge{} | #histogram{}
) -> binary().
prom_metric_name(Name, Unit, Data) ->
    Base = dots_to_underscores(to_bin(Name)),
    WithUnit = maybe_append_unit(Base, Unit),
    maybe_append_total(WithUnit, Data).

-spec maybe_append_unit(binary(), otel_instrument:unit() | undefined) -> binary().
maybe_append_unit(Base, undefined) ->
    Base;
maybe_append_unit(Base, '') ->
    Base;
maybe_append_unit(Base, s) ->
    append_if_missing(Base, <<"_seconds">>);
maybe_append_unit(Base, 'By') ->
    append_if_missing(Base, <<"_bytes">>);
maybe_append_unit(Base, ms) ->
    append_if_missing(Base, <<"_milliseconds">>);
maybe_append_unit(Base, Unit) ->
    UnitBin = dots_to_underscores(to_bin(Unit)),
    case UnitBin of
        <<"{", _/binary>> ->
            %% Annotation units like {request} are dropped per OTel spec
            Base;
        _ ->
            append_if_missing(Base, <<"_", UnitBin/binary>>)
    end.

-spec maybe_append_total(binary(), #sum{} | #gauge{} | #histogram{}) -> binary().
maybe_append_total(Name, #sum{is_monotonic = true}) ->
    append_if_missing(Name, <<"_total">>);
maybe_append_total(Name, _) ->
    Name.

-spec append_if_missing(binary(), binary()) -> binary().
append_if_missing(Bin, Suffix) ->
    SuffixSize = byte_size(Suffix),
    BinSize = byte_size(Bin),
    case BinSize >= SuffixSize of
        true ->
            Tail = binary:part(Bin, BinSize - SuffixSize, SuffixSize),
            case Tail =:= Suffix of
                true -> Bin;
                false -> <<Bin/binary, Suffix/binary>>
            end;
        false ->
            <<Bin/binary, Suffix/binary>>
    end.

-spec prom_label_name(opentelemetry:attribute_key()) -> binary().
prom_label_name(Name) ->
    dots_to_underscores(to_bin(Name)).

prom_label_value(V) when is_binary(V) ->
    escape_label_value(V);
prom_label_value(V) when is_atom(V) ->
    escape_label_value(atom_to_binary(V, utf8));
prom_label_value(V) when is_integer(V) ->
    integer_to_binary(V);
prom_label_value(V) when is_float(V) ->
    float_to_binary(V, [{decimals, 6}, compact]);
prom_label_value(V) ->
    escape_label_value(to_bin(V)).

-spec escape_label_value(binary()) -> binary().
escape_label_value(Bin) ->
    case unicode:characters_to_binary(Bin) of
        Bin -> escape_valid_label_value(Bin);
        _ -> ~"_INVALID_UTF8"
    end.

-spec escape_valid_label_value(binary()) -> binary().
escape_valid_label_value(Bin) ->
    binary:replace(
        binary:replace(
            binary:replace(Bin, <<"\\">>, <<"\\\\">>, [global]),
            <<"\"">>,
            <<"\\\"">>,
            [global]
        ),
        <<"\n">>,
        <<"\\n">>,
        [global]
    ).

-spec dots_to_underscores(binary()) -> binary().
dots_to_underscores(Bin) ->
    binary:replace(Bin, <<".">>, <<"_">>, [global]).

-spec format_number(number() | undefined | infinity) -> binary().
format_number(V) when is_integer(V) ->
    integer_to_binary(V);
format_number(V) when is_float(V) ->
    float_to_binary(V, [{decimals, 6}, compact]);
format_number(undefined) ->
    <<"0">>;
format_number(infinity) ->
    <<"+Inf">>.

-spec to_bin(atom() | iodata() | integer()) -> binary().
to_bin(V) when is_binary(V) -> V;
to_bin(V) when is_atom(V) -> atom_to_binary(V, utf8);
to_bin(V) when is_list(V) -> list_to_binary(V);
to_bin(V) when is_integer(V) -> integer_to_binary(V).

-spec prom_type(#sum{} | #gauge{} | #histogram{}) -> binary().
prom_type(#histogram{}) -> <<"histogram">>;
prom_type(#sum{is_monotonic = true}) -> <<"counter">>;
prom_type(#sum{is_monotonic = false}) -> <<"gauge">>;
prom_type(#gauge{}) -> <<"gauge">>.
