-module(otel_nova_build_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([all/0]).
-export([compile_hooks_succeed_as_a_dependency/1]).

all() -> [compile_hooks_succeed_as_a_dependency].

compile_hooks_succeed_as_a_dependency(Config) ->
    {ok, Terms} = file:consult(filename:join(project_root(), "rebar.config")),
    Hooks = [Cmd || {compile, Cmd} <- proplists:get_value(pre_hooks, Terms, [])],
    DepDir = filename:join(?config(priv_dir, Config), "opentelemetry_nova"),
    ok = filelib:ensure_path(DepDir),
    ?assertEqual([{Cmd, 0} || Cmd <- Hooks], [{Cmd, run_like_rebar3(Cmd, DepDir)} || Cmd <- Hooks]).

run_like_rebar3(Cmd, Dir) ->
    Port = open_port({spawn, Cmd}, [exit_status, {cd, Dir}, stderr_to_stdout, hide]),
    exit_status(Port).

exit_status(Port) ->
    receive
        {Port, {exit_status, Status}} -> Status;
        {Port, {data, _}} -> exit_status(Port)
    after 10000 -> error(hook_timeout)
    end.

project_root() ->
    Source = proplists:get_value(source, ?MODULE:module_info(compile)),
    find_root(filename:dirname(Source)).

find_root("/") ->
    error(project_root_not_found);
find_root(Dir) ->
    case filelib:is_regular(filename:join([Dir, "src", "opentelemetry_nova.app.src"])) of
        true -> Dir;
        false -> find_root(filename:dirname(Dir))
    end.
