-module(otel_nova_prom_server).
-moduledoc """
HTTP listener serving `otel_nova_prom_exporter` output on `/metrics`.

Start it from your supervision tree with `child_spec/1` so the listener is
stopped together with your application:

```erlang
init([]) ->
    Children = [otel_nova_prom_server:child_spec(#{port => 9464})],
    {ok, {#{strategy => one_for_one}, Children}}.
```

The listener binds `127.0.0.1` and speaks HTTP/1.1 only, and `/metrics` has
no authentication. Set `ip` (for example `{0, 0, 0, 0}`) to let a scraper on
another host reach it, and restrict who can connect at the network level.
""".
-behaviour(gen_server).

-export([start_link/1, stop/0, child_spec/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(DEFAULT_PORT, 9464).
-define(DEFAULT_IP, {127, 0, 0, 1}).
-define(LISTENER, otel_nova_prom_listener).

-doc "Starts the server and its listener. `port` defaults to 9464 and `ip` to `{127, 0, 0, 1}`.".
start_link(Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Opts, []).

stop() ->
    gen_server:stop(?MODULE).

-doc "Child specification for running the server under a supervisor.".
-spec child_spec(map()) -> supervisor:child_spec().
child_spec(Opts) ->
    #{
        id => ?MODULE,
        start => {?MODULE, start_link, [Opts]},
        type => worker,
        shutdown => 5000
    }.

%%--------------------------------------------------------------------
%% gen_server callbacks
%%--------------------------------------------------------------------

init(Opts) ->
    process_flag(trap_exit, true),
    Port = maps:get(port, Opts, ?DEFAULT_PORT),
    TransportOpts = [{port, Port}, {ip, maps:get(ip, Opts, ?DEFAULT_IP)}],
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/metrics", otel_nova_prom_handler, []}
        ]}
    ]),
    {ok, Pid} = start_listener(TransportOpts, #{
        env => #{dispatch => Dispatch}, protocols => [http]
    }),
    {ok, #{listener_pid => Pid, port => Port}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    _ = cowboy:stop_listener(?LISTENER),
    ok.

%% The server is registered locally, so a listener that already exists here
%% was left behind by an instance killed before or during terminate/2.
start_listener(TransportOpts, ProtoOpts) ->
    case cowboy:start_clear(?LISTENER, TransportOpts, ProtoOpts) of
        {error, {already_started, _}} ->
            restart_listener(TransportOpts, ProtoOpts);
        {error, already_present} ->
            restart_listener(TransportOpts, ProtoOpts);
        Result ->
            Result
    end.

restart_listener(TransportOpts, ProtoOpts) ->
    _ = cowboy:stop_listener(?LISTENER),
    cowboy:start_clear(?LISTENER, TransportOpts, ProtoOpts).
