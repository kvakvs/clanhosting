%%%-----------------------------------------------------------------------------
%%% @doc Clan functions
%%% @end
%%% Created : 15. Aug 2014 14:23
%%%-----------------------------------------------------------------------------
-module(ch_clan_api).

%% API
-export([clan_info/3]).

%% @doc Возвращает информацию об игроке из кэша или из базы данных Wargaming
-spec clan_info(ClanId :: integer(), Token :: binary(), Lang :: binary())
      -> {reply, proplists:proplist()}.
clan_info(ClanId, Token, Lang) ->
  case ch_clan_cache:find(ClanId) of
    {ok, Clan} ->
      {reply, {bert, dict, Clan}};
    {error, not_found} ->
      Url = ch_http:format_url( ch_conf:wg_api_url("clan", "info")
                              , [ {"access_token", binary_to_list(Token)}
                                , {"application_id", ch_conf:wg_app_id()}
                                , {"clan_id", integer_to_list(ClanId)}
                                , {"language", binary_to_list(Lang)}
                                ]),
      {ok, ClanInfoList} = ch_lib:json_api_request(get, Url),
      %% Данные находятся в info["data"][clan_id]
      ClanInfo1 = proplists:get_value(<<"data">>, ClanInfoList),
      IdBin     = list_to_binary(integer_to_list(ClanId)),
      ClanInfo2 = proplists:get_value(IdBin, ClanInfo1),
      ClanInfo  = ch_lib:proplist_to_bert_dict(ClanInfo2, []),
      ch_clan_cache:new(ClanId, ClanInfo),
      {reply, {bert, dict, ClanInfo}}
  end.

%%%-----------------------------------------------------------------------------
%%% INTERNAL
%%%-----------------------------------------------------------------------------
