%%%-------------------------------------------------------------------
%%% @doc Various application related modules
%%% @end
%%% Created : 24. Aug 2014
%%%-------------------------------------------------------------------
-module(libe_app).

%% API
-export([ensure_started/1]).

ensure_started(Apps) when is_list(Apps) ->
  lists:map(fun ensure_started/1, Apps);
ensure_started(App) when is_atom(App) ->
  ensure_started_1(App, 25).

%% @private
ensure_started_1(_, 0) -> erlang:error({?MODULE, ensure_started, retries_count});
ensure_started_1(App, Retries) ->
  case application:start(App) of
    {error, {not_started, Dependency}} ->
      ensure_started_1(Dependency, Retries-1),
      ensure_started_1(App, Retries-1);
    _ -> %{error, {already_started, _App}} ->
      ok
  end.
