-module(otel_nova_test_ws).
-behaviour(nova_websocket).

-export([init/1, websocket_handle/2, websocket_info/2]).

init(ControllerData) ->
    {ok, ControllerData}.

websocket_handle({text, Text}, ControllerData) ->
    {reply, {text, Text}, ControllerData};
websocket_handle(_Frame, ControllerData) ->
    {ok, ControllerData}.

websocket_info(_Info, ControllerData) ->
    {ok, ControllerData}.
