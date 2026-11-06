-module(otel_nova_test_app).
-behaviour(application).
-behaviour(supervisor).

-export([start/2, stop/1, init/1]).

start(_Type, _Args) ->
    {ok, Mode} = application:get_env(otel_nova_test_app, mode),
    {ok, PromOpts} = application:get_env(otel_nova_test_app, prometheus),
    {ok, Sup} = supervisor:start_link(?MODULE, {Mode, PromOpts}),
    case Mode of
        setup -> ok = opentelemetry_nova:setup(#{prometheus => PromOpts});
        supervised -> ok
    end,
    {ok, Sup}.

stop(_State) ->
    ok.

init({supervised, PromOpts}) ->
    {ok, {#{strategy => one_for_one}, [otel_nova_prom_server:child_spec(PromOpts)]}};
init({setup, _PromOpts}) ->
    {ok, {#{strategy => one_for_one}, []}}.
