-module(fake_controller).
-export([index/1]).

index(_Req) ->
    {ok, #{message => <<"hello">>}}.
