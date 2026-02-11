-module(otel_nova_prom_server).
-behaviour(gen_server).

-export([start_link/1, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(DEFAULT_PORT, 9464).

start_link(Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Opts, []).

stop() ->
    gen_server:stop(?MODULE).

%%--------------------------------------------------------------------
%% gen_server callbacks
%%--------------------------------------------------------------------

init(Opts) ->
    Port = maps:get(port, Opts, ?DEFAULT_PORT),
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/metrics", otel_nova_prom_handler, []}
        ]}
    ]),
    {ok, Pid} = cowboy:start_clear(otel_nova_prom_listener, [{port, Port}], #{
        env => #{dispatch => Dispatch}
    }),
    {ok, #{listener_pid => Pid, port => Port}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    cowboy:stop_listener(otel_nova_prom_listener),
    ok.
