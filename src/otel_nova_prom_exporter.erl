-module(otel_nova_prom_exporter).

-export([init/1, export/4, force_flush/0, shutdown/0]).
-export([get_metrics/0]).

-include_lib("opentelemetry_experimental/include/otel_metrics.hrl").

-define(TABLE, otel_nova_prom_metrics).

init(_Config) ->
    case ets:info(?TABLE) of
        undefined ->
            ets:new(?TABLE, [named_table, public, set, {read_concurrency, true}]);
        _ ->
            ok
    end,
    {ok, #{}}.

export(metrics, Metrics, _Resource, _State) ->
    lists:foreach(fun accumulate_metric/1, Metrics),
    Output = render_all(),
    ets:insert(?TABLE, {metrics_output, Output}),
    ok.

force_flush() ->
    ok.

shutdown() ->
    case ets:info(?TABLE) of
        undefined -> ok;
        _ -> ets:delete(?TABLE)
    end,
    ok.

get_metrics() ->
    case ets:info(?TABLE) of
        undefined -> <<>>;
        _ ->
            case ets:lookup(?TABLE, metrics_output) of
                [{metrics_output, Output}] -> Output;
                [] -> <<>>
            end
    end.

%%--------------------------------------------------------------------
%% Internal - delta to cumulative accumulation
%%--------------------------------------------------------------------

accumulate_metric(#metric{name = Name, description = Desc, unit = Unit, data = Data}) ->
    PromName = prom_metric_name(Name, Unit, Data),
    Type = prom_type(Data),
    ets:insert(?TABLE, {metric_meta(PromName), Desc, Type, Unit, Data}),
    accumulate_data(PromName, Data).

accumulate_data(PromName, #histogram{datapoints = Datapoints}) ->
    lists:foreach(fun(DP) -> accumulate_histogram(PromName, DP) end, Datapoints);
accumulate_data(PromName, #sum{datapoints = Datapoints, is_monotonic = true}) ->
    lists:foreach(fun(DP) -> accumulate_counter(PromName, DP) end, Datapoints);
accumulate_data(PromName, #sum{datapoints = Datapoints, is_monotonic = false}) ->
    %% Updown counters: store latest value, don't accumulate
    lists:foreach(fun(DP) -> store_gauge(PromName, DP) end, Datapoints);
accumulate_data(PromName, #gauge{datapoints = Datapoints}) ->
    lists:foreach(fun(DP) -> store_gauge(PromName, DP) end, Datapoints).

accumulate_histogram(PromName, #histogram_datapoint{attributes = Attrs,
                                                     count = Count,
                                                     sum = Sum,
                                                     bucket_counts = BucketCounts,
                                                     explicit_bounds = Bounds}) ->
    Key = {dp, PromName, normalize_attrs(Attrs)},
    case ets:lookup(?TABLE, Key) of
        [{Key, {hist, PrevCount, PrevSum, PrevBuckets, B}}] ->
            NewBuckets = add_lists(PrevBuckets, BucketCounts),
            ets:insert(?TABLE, {Key, {hist, PrevCount + Count, add_num(PrevSum, Sum), NewBuckets, B}});
        [] ->
            ets:insert(?TABLE, {Key, {hist, Count, Sum, BucketCounts, Bounds}})
    end.

accumulate_counter(PromName, #datapoint{attributes = Attrs, value = Value}) ->
    Key = {dp, PromName, normalize_attrs(Attrs)},
    case ets:lookup(?TABLE, Key) of
        [{Key, {counter, PrevValue}}] ->
            ets:insert(?TABLE, {Key, {counter, add_num(PrevValue, Value)}});
        [] ->
            ets:insert(?TABLE, {Key, {counter, Value}})
    end.

store_gauge(PromName, #datapoint{attributes = Attrs, value = Value}) ->
    Key = {dp, PromName, normalize_attrs(Attrs)},
    ets:insert(?TABLE, {Key, {gauge, Value}}).

add_num(A, B) when is_number(A), is_number(B) -> A + B;
add_num(undefined, B) -> B;
add_num(A, undefined) -> A;
add_num(_, _) -> 0.

add_lists(A, B) ->
    lists:zipwith(fun(X, Y) -> X + Y end, A, B).

normalize_attrs(Attrs) ->
    lists:sort(maps:to_list(Attrs)).

metric_meta(PromName) -> {meta, PromName}.

%%--------------------------------------------------------------------
%% Internal - render accumulated state
%%--------------------------------------------------------------------

render_all() ->
    Metas = ets:match(?TABLE, {{meta, '$1'}, '$2', '$3', '_', '_'}),
    Lines = lists:flatmap(fun([PromName, Desc, Type]) ->
        render_metric(PromName, Desc, Type)
    end, lists:sort(Metas)),
    iolist_to_binary(Lines).

render_metric(PromName, Desc, Type) ->
    Header = [
        <<"# HELP ">>, PromName, <<" ">>, to_bin(Desc), <<"\n">>,
        <<"# TYPE ">>, PromName, <<" ">>, Type, <<"\n">>
    ],
    DPs = ets:match(?TABLE, {{dp, PromName, '$1'}, '$2'}),
    Body = lists:flatmap(fun([AttrList, Data]) ->
        Attrs = maps:from_list(AttrList),
        render_datapoint(PromName, Attrs, Data)
    end, lists:sort(DPs)),
    case Body of
        [] -> Header;
        _ -> Header ++ Body
    end.

render_datapoint(Name, Attrs, {hist, Count, Sum, BucketCounts, Bounds}) ->
    Labels = format_labels(Attrs),
    BucketLines = format_buckets(Name, Labels, BucketCounts, Bounds),
    SumLine = [Name, <<"_sum">>, Labels, <<" ">>, format_number(Sum), <<"\n">>],
    CountLine = [Name, <<"_count">>, Labels, <<" ">>, format_number(Count), <<"\n">>],
    BucketLines ++ [SumLine, CountLine];
render_datapoint(Name, Attrs, {counter, Value}) ->
    Labels = format_labels(Attrs),
    [[Name, Labels, <<" ">>, format_number(Value), <<"\n">>]];
render_datapoint(Name, Attrs, {gauge, Value}) ->
    Labels = format_labels(Attrs),
    [[Name, Labels, <<" ">>, format_number(Value), <<"\n">>]].

format_buckets(Name, Labels, BucketCounts, Bounds) ->
    format_buckets(Name, Labels, BucketCounts, Bounds, 0, []).

format_buckets(Name, Labels, [Count | RestCounts], [Bound | RestBounds], Cumulative, Acc) ->
    CumCount = Cumulative + Count,
    Le = format_number(Bound),
    Line = [Name, <<"_bucket">>, insert_le_label(Labels, Le), <<" ">>, format_number(CumCount), <<"\n">>],
    format_buckets(Name, Labels, RestCounts, RestBounds, CumCount, Acc ++ [Line]);
format_buckets(Name, Labels, [Count | _], [], Cumulative, Acc) ->
    CumCount = Cumulative + Count,
    Line = [Name, <<"_bucket">>, insert_le_label(Labels, <<"+Inf">>), <<" ">>, format_number(CumCount), <<"\n">>],
    Acc ++ [Line];
format_buckets(_Name, _Labels, [], _, _Cumulative, Acc) ->
    Acc.

%%--------------------------------------------------------------------
%% Internal - label formatting
%%--------------------------------------------------------------------

format_labels(Attrs) when map_size(Attrs) =:= 0 ->
    <<>>;
format_labels(Attrs) ->
    Pairs = lists:sort(maps:fold(fun(K, V, Acc) ->
        [{prom_label_name(K), prom_label_value(V)} | Acc]
    end, [], Attrs)),
    Inner = lists:join(<<",">>, [
        [LK, <<"=\"">>, LV, <<"\"">>] || {LK, LV} <- Pairs
    ]),
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

prom_metric_name(Name, Unit, Data) ->
    Base = dots_to_underscores(to_bin(Name)),
    WithUnit = maybe_append_unit(Base, Unit),
    maybe_append_total(WithUnit, Data).

maybe_append_unit(Base, undefined) -> Base;
maybe_append_unit(Base, '') -> Base;
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

maybe_append_total(Name, #sum{is_monotonic = true}) ->
    append_if_missing(Name, <<"_total">>);
maybe_append_total(Name, _) ->
    Name.

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

escape_label_value(Bin) ->
    binary:replace(
        binary:replace(
            binary:replace(Bin, <<"\\">>, <<"\\\\">>, [global]),
            <<"\"">>, <<"\\\"">>, [global]),
        <<"\n">>, <<"\\n">>, [global]).

dots_to_underscores(Bin) ->
    binary:replace(Bin, <<".">>, <<"_">>, [global]).

format_number(V) when is_integer(V) ->
    integer_to_binary(V);
format_number(V) when is_float(V) ->
    float_to_binary(V, [{decimals, 6}, compact]);
format_number(undefined) ->
    <<"0">>;
format_number(infinity) ->
    <<"+Inf">>.

to_bin(V) when is_binary(V) -> V;
to_bin(V) when is_atom(V) -> atom_to_binary(V, utf8);
to_bin(V) when is_list(V) -> list_to_binary(V);
to_bin(V) when is_integer(V) -> integer_to_binary(V).

prom_type(#histogram{}) -> <<"histogram">>;
prom_type(#sum{is_monotonic = true}) -> <<"counter">>;
prom_type(#sum{is_monotonic = false}) -> <<"gauge">>;
prom_type(#gauge{}) -> <<"gauge">>.
