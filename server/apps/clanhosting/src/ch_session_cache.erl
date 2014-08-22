%%%-------------------------------------------------------------------
%%% @doc Session management and cache
%%% @end
%%% Created : 16. Aug 2014
%%%-------------------------------------------------------------------
-module(ch_session_cache).

%% API
-export([init/0, new/2, find/1]).

init() ->
  ets:new(?MODULE, [named_table, public]).

%% TODO: session expiration
new(Key, Props) when is_integer(Key) ->
  ets:insert(?MODULE, {Key, Props}).

find(Key) when is_integer(Key) ->
  case ets:lookup(?MODULE, Key) of
    [] -> {error, not_found};
    [Session] -> {ok, Session}
  end.
