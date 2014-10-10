%%%-----------------------------------------------------------------------------
%%% @doc Site functions
%%% @end
%%% Created : 15. Aug 2014 14:23
%%%-----------------------------------------------------------------------------
-module(ch_site_api).

%% API
-export([exists/1, read/1, update/2]).

-spec exists(ClanId :: integer()) -> {reply, {bert, boolean()}}.
exists(ClanId) ->
  case riak_pool:with_worker(fun(Worker) ->
                             ch_db:read_map_object(Worker, {site, ClanId})
                           end) of
    {ok, _Value} -> {reply, 1}; %{bert, true}};
    {error, _E}  -> {reply, 0}  %{bert, false}}
  end.

-spec update(ClanId :: integer(), Fields :: dict()) -> {reply, ok}.
update(ClanId, Fields0) ->
  Fields = dict:to_list(Fields0),
  lists:foreach(fun({K,V}) -> on_field_updated(ClanId, K, V) end, Fields),
  R = riak_pool:with_worker(fun(Worker) ->
                      ch_db:update_map(Worker, {site, ClanId}, Fields, [])
                    end),
  {reply, R}.

-spec read(ClanId :: integer()) -> {reply, {bert, dict, proplists:proplist()}}.
read(ClanId) ->
  case riak_pool:with_worker(fun(Worker) ->
                             ch_db:read_map_object(Worker, {site, ClanId})
                           end) of
    {ok, MapObject} ->
      Map = ch_db:map_object_to_orddict(MapObject),
      {reply, {bert, dict, Map}};
    {error, _E} -> {reply, {bert, nil}}
  end.

%%%-----------------------------------------------------------------------------
%%% INTERNAL
%%%-----------------------------------------------------------------------------


%% @private
%% @doc Записывает изменения в конфигурацию домена клана. Даёт сигнал
%% пересоздания конфига nginx.
on_field_updated(ClanId, <<"custom_domain">>, _V) ->
  ch_db:add_to_index({hosting, <<(ClanId rem ?HOSTING_INDEX_FRAGMENTS):16>>}
                    , [ClanId]),
  ok;
on_field_updated(ClanId, <<"free_subdomain">>, _V) ->
  ch_hosting:queue_change_for_clanid(ClanId, free_subdomain, V),
  ok;
on_field_updated(_ClanId, _K, _V) -> ok.
