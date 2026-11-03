-module(otel_nova_test_router).
-behaviour(cowboy_middleware).

-export([execute/2]).

execute(Req = #{path := Path}, Env) ->
    Plugins = [{pre_request, [{fun otel_nova_plugin:pre_request/4, #{}}]}],
    Env1 = Env#{app => test_app, callback => fun fake_controller:index/1},
    {ok, Routes} = nova_routing_trie:insert(~"/assets/[...]", '_', static, nova_routing_trie:new()),
    case nova_routing_trie:find('_', Path, '_', Routes) of
        {ok, Bindings, static, PathInfo} ->
            Req1 = Req#{bindings => Bindings, extra_state => #{pathinfo => PathInfo}},
            {ok, Req1#{plugins => Plugins}, Env1};
        _ ->
            {ok, Req#{plugins => Plugins}, Env1}
    end.
