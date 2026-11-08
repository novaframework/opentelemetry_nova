-module(otel_nova_test_router).
-behaviour(cowboy_middleware).

-export([execute/2]).

execute(Req = #{path := <<"/nova/", Action/binary>>}, Env) ->
    CrashPlugin = [{fun otel_nova_test_controller:crash_plugin/4, #{}} || Action =:= ~"plugin_crash"],
    Plugins = [
        {pre_request, [{fun otel_nova_plugin:pre_request/4, #{}} | CrashPlugin]},
        {post_request, [{fun otel_nova_plugin:post_request/4, #{}}]}
    ],
    Secure =
        case Action of
            ~"security_crash" -> fun otel_nova_test_controller:crash_security/1;
            _ -> false
        end,
    {ok, Req#{plugins => Plugins}, Env#{
        app => test_app,
        callback => nova_callback(Action),
        secure => Secure,
        controller_data => #{}
    }};
execute(Req = #{path := Path}, Env) ->
    Plugins = [{pre_request, [{fun otel_nova_plugin:pre_request/4, #{}}]}],
    Env1 = Env#{app => test_app, callback => fun fake_controller:index/1},
    {ok, Routes} = nova_routing_trie:insert(~"/assets/[...]", '_', static, nova_routing_trie:new()),
    case nova_routing_trie:find('_', Path, '_', Routes) of
        {ok, Bindings, static, PathInfo} ->
            Req1 = Req#{bindings => Bindings, extra_state => #{pathinfo => PathInfo}},
            {ok, Req1#{plugins => Plugins}, Env1};
        _ when Path =:= ~"/plugin_crash" ->
            {ok, Req#{plugins => Plugins}, Env1#{app => ~"not_an_atom"}};
        _ ->
            {ok, Req#{plugins => Plugins}, Env1}
    end.

nova_callback(~"crash") -> fun otel_nova_test_controller:crash/1;
nova_callback(~"crash_tuple") -> fun otel_nova_test_controller:crash_tuple/1;
nova_callback(~"throw_status") -> fun otel_nova_test_controller:throw_status/1;
nova_callback(_) -> fun otel_nova_test_controller:ok/1.
