-module(otel_nova_plugin).
%% Implements nova_plugin behaviour callbacks.
%% The -behaviour directive is omitted because nova is only a test dependency.

-export([pre_request/4, post_request/4, plugin_info/0]).

-include_lib("opentelemetry_api/include/otel_tracer.hrl").

pre_request(Req, Env, _Opts, State) ->
    Method = cowboy_req:method(Req),
    Route = reconstruct_route(Req),

    ?set_attribute('http.route', Route),
    erlang:put(otel_nova_http_route, Route),
    ?update_name(<<Method/binary, " ", Route/binary>>),

    case extract_callback_info(Env) of
        {App, Controller, Action} ->
            ?set_attributes(#{
                'nova.app' => atom_to_binary(App, utf8),
                'nova.controller' => atom_to_binary(Controller, utf8),
                'nova.action' => atom_to_binary(Action, utf8)
            });
        undefined ->
            ok
    end,
    {ok, Req, State}.

post_request(Req, _Env, _Opts, State) ->
    {ok, Req, State}.

plugin_info() ->
    #{
        title => <<"otel_nova_plugin">>,
        version => <<"0.1.0">>,
        url => <<"https://github.com/novaframework/opentelemetry_nova">>,
        authors => [<<"OpenTelemetry Nova contributors">>],
        description => <<"Enriches OpenTelemetry spans with Nova routing information">>
    }.

%% Internal

reconstruct_route(#{bindings := Bindings, path := Path}) when map_size(Bindings) > 0 ->
    Segments = binary:split(Path, <<"/">>, [global]),
    InverseBindings = maps:fold(
        fun(K, V, Acc) ->
            Acc#{V => K}
        end,
        #{},
        Bindings
    ),
    Replaced = [
        case maps:find(Seg, InverseBindings) of
            {ok, Key} -> <<":", Key/binary>>;
            error -> Seg
        end
     || Seg <- Segments
    ],
    iolist_to_binary(lists:join(<<"/">>, Replaced));
reconstruct_route(#{path := Path}) ->
    Path.

extract_callback_info(#{app := App, callback := Fun}) when is_function(Fun) ->
    case {erlang:fun_info(Fun, module), erlang:fun_info(Fun, name)} of
        {{module, Mod}, {name, Name}} ->
            {App, Mod, Name};
        _ ->
            undefined
    end;
extract_callback_info(_) ->
    undefined.
