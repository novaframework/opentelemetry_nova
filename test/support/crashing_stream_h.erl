-module(crashing_stream_h).
-behaviour(cowboy_stream).

-export([init/3, data/4, info/3, terminate/3, early_error/5]).

init(_StreamID, _Req, _Opts) ->
    error(stream_init_boom).

data(_StreamID, _IsFin, _Data, State) ->
    {[], State}.

info(_StreamID, _Info, State) ->
    {[], State}.

terminate(_StreamID, _Reason, _State) ->
    ok.

early_error(_StreamID, _Reason, _PartialReq, Resp, _Opts) ->
    Resp.
