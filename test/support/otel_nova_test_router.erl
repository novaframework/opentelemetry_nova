-module(otel_nova_test_router).
-behaviour(cowboy_middleware).

-export([execute/2]).

execute(Req, Env) ->
    Plugins = [{pre_request, [{fun otel_nova_plugin:pre_request/4, #{}}]}],
    {ok, Req#{plugins => Plugins}, Env#{app => test_app, callback => fun fake_controller:index/1}}.
