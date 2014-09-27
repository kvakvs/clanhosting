%%%-------------------------------------------------------------------
%%% @doc Configuration data
%%% @end
%%% Created : 15. Aug 2014 14:40
%%%-------------------------------------------------------------------
-module(ch_conf).

%% API
-export([wg_api_url/2, wg_app_id/0]).

%% @doc Group e.g. "account", Func e.g. "info". Produces iolist
wg_api_url(Group, Func) ->
  ["https://api.worldoftanks.ru/wot/", Group, $/, Func, $/].

wg_app_id() ->
  "a2725c4b342fdf9977ffe92a2b84f4ee".