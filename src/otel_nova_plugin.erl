-module(otel_nova_plugin).
-behaviour(nova_plugin).

-export([pre_request/4, post_request/4, plugin_info/0]).

-include_lib("opentelemetry_api/include/otel_tracer.hrl").

pre_request(Req, Env, _Opts, State) ->
    Method = cowboy_req:method(Req),

    case extract_callback_info(Env) of
        {App, Controller, Action} ->
            ControllerBin = atom_to_binary(Controller, utf8),
            ActionBin = atom_to_binary(Action, utf8),
            ?set_attributes(#{
                'nova.app' => atom_to_binary(App, utf8),
                'nova.controller' => ControllerBin,
                'nova.action' => ActionBin
            }),
            SpanName = <<Method/binary, " ", ControllerBin/binary, ":", ActionBin/binary>>,
            ?update_name(SpanName);
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

extract_callback_info(#{app := App, callback := Fun}) when is_function(Fun) ->
    case {erlang:fun_info(Fun, module), erlang:fun_info(Fun, name)} of
        {{module, Mod}, {name, Name}} ->
            {App, Mod, Name};
        _ ->
            undefined
    end;
extract_callback_info(_) ->
    undefined.
