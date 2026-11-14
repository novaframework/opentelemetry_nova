-module(otel_nova_build_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([all/0]).
-export([
    compile_hooks_succeed_as_a_dependency/1,
    hook_does_nothing_without_the_header/1,
    hook_patches_the_header_once/1,
    hook_patches_a_later_release_that_still_needs_it/1,
    hook_fails_when_upstream_no_longer_needs_it/1,
    hook_matches_the_locked_release/1
]).

-define(HEADER, "_build/default/lib/opentelemetry_experimental/include/otel_metrics.hrl").
-define(SCRIPT, "scripts/patch_otel_metrics_hrl.sh").
-define(UNPATCHED, ~"start_time :: integer() | {const, eqwalizer:dynamic()} | undefined,\n").
-define(PATCHED, ~"start_time :: integer() | {const, dynamic()} | undefined,\n").

all() ->
    [
        compile_hooks_succeed_as_a_dependency,
        hook_does_nothing_without_the_header,
        hook_patches_the_header_once,
        hook_patches_a_later_release_that_still_needs_it,
        hook_fails_when_upstream_no_longer_needs_it,
        hook_matches_the_locked_release
    ].

compile_hooks_succeed_as_a_dependency(Config) ->
    {ok, Terms} = file:consult(filename:join(project_root(), "rebar.config")),
    Hooks = [Cmd || {compile, Cmd} <- proplists:get_value(pre_hooks, Terms, [])],
    DepDir = filename:join(?config(priv_dir, Config), "opentelemetry_nova"),
    ok = filelib:ensure_path(DepDir),
    ?assertEqual([{Cmd, 0} || Cmd <- Hooks], [{Cmd, run_like_rebar3(Cmd, DepDir)} || Cmd <- Hooks]).

%% A git dependency or a checkout has the script but not the header.
hook_does_nothing_without_the_header(Config) ->
    Dir = filename:join(?config(priv_dir, Config), "dependency"),
    ok = filelib:ensure_path(Dir),
    ?assertEqual({0, <<>>}, run_script(Dir)).

hook_patches_the_header_once(Config) ->
    Dir = project_copy(Config, "patch", ?UNPATCHED, ~"0.6.0"),
    ?assertMatch({0, _}, run_script(Dir)),
    ?assertEqual({ok, ?PATCHED}, file:read_file(filename:join(Dir, ?HEADER))),
    ?assertMatch({0, _}, run_script(Dir)),
    ?assertEqual({ok, ?PATCHED}, file:read_file(filename:join(Dir, ?HEADER))).

hook_patches_a_later_release_that_still_needs_it(Config) ->
    Dir = project_copy(Config, "later", ?UNPATCHED, ~"0.6.1"),
    ?assertMatch({0, _}, run_script(Dir)),
    ?assertEqual({ok, ?PATCHED}, file:read_file(filename:join(Dir, ?HEADER))).

hook_fails_when_upstream_no_longer_needs_it(Config) ->
    Dir = project_copy(Config, "fixed", ?PATCHED, ~"0.6.1"),
    {Status, Output} = run_script(Dir),
    ?assertNotEqual(0, Status),
    ?assertMatch({match, _}, re:run(Output, ~"Remove\\s+scripts/patch_otel_metrics_hrl.sh")),
    ?assertEqual({ok, ?PATCHED}, file:read_file(filename:join(Dir, ?HEADER))).

%% The release the script treats as already patched is the one in rebar.lock.
%% When the lock moves, check the header of the new release and update or
%% remove the script.
hook_matches_the_locked_release(_Config) ->
    {ok, Lock} = file:read_file(filename:join(project_root(), "rebar.lock")),
    {ok, Script} = file:read_file(filename:join(project_root(), ?SCRIPT)),
    ?assertNotEqual(nomatch, binary:match(Lock, lock_line(~"0.6.0"))),
    ?assertNotEqual(nomatch, binary:match(Script, lock_line(~"0.6.0"))).

project_copy(Config, Name, Header, Version) ->
    Dir = filename:join(?config(priv_dir, Config), Name),
    ok = filelib:ensure_dir(filename:join(Dir, ?HEADER)),
    ok = file:write_file(filename:join(Dir, ?HEADER), Header),
    ok = file:write_file(filename:join(Dir, "rebar.lock"), [lock_line(Version), ~",0}\n"]),
    Dir.

lock_line(Version) ->
    <<"{pkg,<<\"opentelemetry_experimental\">>,<<\"", Version/binary, "\">>}">>.

run_script(Dir) ->
    Script = filename:join(project_root(), ?SCRIPT),
    Port = open_port({spawn_executable, os:find_executable("sh")}, [
        {args, [Script]}, exit_status, {cd, Dir}, stderr_to_stdout, binary, hide
    ]),
    collect(Port, <<>>).

run_like_rebar3(Cmd, Dir) ->
    Port = open_port({spawn, Cmd}, [exit_status, {cd, Dir}, stderr_to_stdout, binary, hide]),
    {Status, _Output} = collect(Port, <<>>),
    Status.

collect(Port, Output) ->
    receive
        {Port, {exit_status, Status}} -> {Status, Output};
        {Port, {data, Data}} -> collect(Port, <<Output/binary, Data/binary>>)
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
