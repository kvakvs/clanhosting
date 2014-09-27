%%%-----------------------------------------------------------------------------
%%% @doc Альянсы. В базе данных информация об альянсах взаимная, то есть
%%% симметричная копия хранится с двух сторон альянса.
%%% @end
%%% Created : 26. Aug 2014
%%%-----------------------------------------------------------------------------
-module(ch_clan_api).

%% API
-export([add_alliance/2
        , are_in_alliance/2
        , break_alliance/2
        , clan_info/3
        , clan_info/1
        , delete_alliance_request/2
        , list_alliances/1
        , list_alliance_requests/1
        , request_alliance/2
        , search_clans/2
        ]).

%% @doc Возвращает информацию об игроке из кэша или из базы данных Wargaming
-spec clan_info(ClanId :: integer(), Token :: binary(), Lang :: binary())
      -> {reply, {bert, dict, proplists:proplist()}}.
clan_info(ClanId, Token, Lang) ->
  QueryFun = fun() ->
            Url = ch_http:format_url( ch_conf:wg_api_url("clan", "info")
                                    , [ {"access_token", libe_types:as_string(Token)}
                                      , {"application_id", ch_conf:wg_app_id()}
                                      , {"clan_id", libe_types:as_string(ClanId)}
                                      , {"language", libe_types:as_string(Lang)}
                                      ]),
            {ok, ClanInfoList} = ch_lib:json_api_request(get, Url),
            %% Данные находятся в info["data"][clan_id]
            ClanInfo1 = proplists:get_value(<<"data">>, ClanInfoList),
            IdBin     = list_to_binary(integer_to_list(ClanId)),
            ClanInfo2 = proplists:get_value(IdBin, ClanInfo1),
            ch_lib:proplist_to_bert_dict(ClanInfo2, [])
          end,
  ClanInfo = ch_lib:memoize(ch_clan_cache, ClanId, QueryFun),
  {reply, {bert, dict, ClanInfo}}.

%% @doc Возвращает информацию об игроке из кэша или из базы данных Wargaming
-spec clan_info(ClanId :: integer())
      -> {reply, {bert, dict, proplists:proplist()}}.
clan_info(ClanId) ->
  QueryFun = fun() ->
    Url = ch_http:format_url( ch_conf:wg_api_url("clan", "info")
      , [ {"application_id", ch_conf:wg_app_id()}
        , {"clan_id", libe_types:as_string(ClanId)}
        ]),
    {ok, ClanInfoList} = ch_lib:json_api_request(get, Url),
    %% Данные находятся в info["data"][clan_id]
    ClanInfo1 = proplists:get_value(<<"data">>, ClanInfoList),
    IdBin     = list_to_binary(integer_to_list(ClanId)),
    ClanInfo2 = proplists:get_value(IdBin, ClanInfo1),
    ch_lib:proplist_to_bert_dict(ClanInfo2, [])
  end,
  ClanInfo = ch_lib:memoize(ch_clan_cache, ClanId, QueryFun),
  {reply, {bert, dict, ClanInfo}}.

%% @doc Возвращает информацию об игроке из кэша или из базы данных Wargaming
-spec search_clans(Query :: binary(), Lang :: binary())
      -> {reply, list({bert, dict, proplists:proplist()})}.
search_clans(Query, Lang) ->
  QueryFun = fun() ->
    Url = ch_http:format_url( ch_conf:wg_api_url("clan", "list")
      , [ {"application_id", ch_conf:wg_app_id()}
        , {"search", libe_types:as_string(Query)}
        , {"language", libe_types:as_string(Lang)}
        ]),
    {ok, ClanInfoList} = ch_lib:json_api_request(get, Url),
    %% Данные находятся в info["data"][clan_id]
    ClansFound = proplists:get_value(<<"data">>, ClanInfoList),
    lists:map(fun(C) -> ch_lib:proplist_to_bert_dict(C, []) end, ClansFound)
  end,
  Clans = ch_lib:memoize(ch_clan_search_cache, Query, QueryFun),
  {reply, lists:map(fun(C) -> {bert, dict, C} end, Clans)}.

%% @doc Ошибки быть не может, пустой список если данных в БД нет
list_alliances(ClanId) ->
  {reply, gen_read_index(alliance, ClanId)}.

add_alliance(ClanId1, ClanId2) ->
  ch_db:add_to_index({alliance, ClanId1}, ClanId2),
  ch_db:add_to_index({alliance, ClanId2}, ClanId1),
  {reply, ok}.

break_alliance(ClanId1, ClanId2) ->
  ch_db:remove_from_index({alliance, ClanId1}, ClanId2),
  ch_db:remove_from_index({alliance, ClanId2}, ClanId1),
  {reply, ok}.

list_alliance_requests(ClanId) ->
  {reply, gen_read_index(alliancereq, ClanId)}.

request_alliance(ClanId1, ClanId2) ->
  ch_db:add_to_index({alliancereq, ClanId1}, ClanId2),
  {reply, ok}.

delete_alliance_request(ClanId1, ClanId2) ->
  ch_db:remove_from_index({alliancereq, ClanId1}, ClanId2),
  {reply, ok}.

are_in_alliance(ClanId1, ClanId2) ->
  case gen_read_index(alliance, ClanId1) of
    {error, _}      -> {reply, 0};
    {ok, AlliedClans} ->
      case lists:member(ClanId2, AlliedClans) of
        true  -> {reply, 1};
        false -> {reply, 0}
      end
  end.

%%%-----------------------------------------------------------------------------
%%% INTERNAL
%%%-----------------------------------------------------------------------------

%% -spec gen_add_to_index(RecType :: atom(), Key1 :: integer()
%%                       , AddValues :: [ch_db:set_value()]) -> {reply, ok}.
%% gen_add_to_index(RecType, Key1, AddValues) when is_atom(RecType) ->
%%   {reply, ch_db:add_to_index({RecType, Key1}, AddValues)}.
%%
%% -spec gen_remove_from_index(RecType :: atom(), Key1 :: integer()
%%                            , DeleteValues :: [ch_db:set_value()]) -> {reply, ok}.
%% gen_remove_from_index(RecType, Key1, DeleteValues) when is_atom(RecType) ->
%%   {reply, ch_db:remove_from_index({RecType, Key1}, DeleteValues)}.

-spec gen_read_index(RecType :: atom(), Key1 :: integer())
      -> {reply, list(ch_db:set_value())}.
gen_read_index(RecType, Key1) when is_atom(RecType) ->
  case riak_pool:with_worker(fun(Worker) ->
    ch_db:read_set_object(Worker, {RecType, Key1})
  end) of
    {ok, SetObject} -> riakc_set:value(SetObject);
    {error, _E}     -> []
  end.
