%%%-----------------------------------------------------------------------------
%%% @doc Hosting nginx config and restart handling
%%% @end
%%% Created : 09. Oct 2014 22:15
%%%-------------------------------------------------------------------
-module(ch_hosting).

%% API
-export([queue_change_for_clanid/3]).

-define(HOSTING_INDEX_FRAGMENTS, 32).

queue_change_for_clanid(ClanId, custom_domain, _Value) ->
  set_refresh_mark(ClanId);
queue_change_for_clanid(ClanId, free_subdomain, _Value) ->
  set_refresh_mark(ClanId).

%% @private
%% @doc Делим диапазон всех кланов на ?HOSTING_INDEX_FRAGMENTS частей, и в одну
%% из них ставим отметку, что сайт обновился. Отдельный процесс регулярно
%% выбирает отметки и обновляет конфигурацию хостинга.
set_refresh_mark(ClanId) ->
  ch_db:add_to_index({hosting, <<(ClanId rem ?HOSTING_INDEX_FRAGMENTS):16>>}
                    , [ClanId]).
3
