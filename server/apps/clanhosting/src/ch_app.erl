-module(ch_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1, start/0]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() ->
  ok = lager:start(),
  libe_app:ensure_started([
      ssl, crypto, public_key, lhttpc, riak_pool,
      flake,
      clanhosting
    ]).

start(_StartType, _StartArgs) ->
  ch_session_cache:init(),
  ch_clan_cache:init(),
  start_listening(),
  ch_sup:start_link().

stop(_State) ->
  ok.

start_listening() ->
  ok = application:start(ranch),
  {ok, _} = ranch:start_listener(ch_ranch_listener, 10, ranch_tcp,
                                [{port, 10000}], ch_protocol, []).