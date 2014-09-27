%%%-----------------------------------------------------------------------------
%%% @doc Альянсы. В базе данных информация об альянсах взаимная, то есть
%%% симметричная копия хранится с двух сторон альянса.
%%% @end
%%% Created : 26. Aug 2014
%%%-----------------------------------------------------------------------------
-module(ch_clan_api).

%% API
-export([ clan_info/3
        , list_alliances/1
        , add_alliance/2
        , break_alliance/2
        , list_alliance_requests/1
        , request_alliance/2
        , delete_alliance_request/2
        ]).

%% @doc Возвращает информацию об игроке из кэша или из базы данных Wargaming
-spec clan_info(ClanId :: integer(), Token :: binary(), Lang :: binary())
      -> {reply, {bert, dict, proplists:proplist()}}.
clan_info(ClanId, Token, Lang) ->
  QueryFun = fun() ->
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
            ch_lib:proplist_to_bert_dict(ClanInfo2, [])
          end,
  ClanInfo = ch_lib:memoize(ch_clan_cache, ClanId, QueryFun),
  {reply, {bert, dict, ClanInfo}}.

%% @doc Ошибки быть не может, пустой список если данных в БД нет
list_alliances(ClanId) ->
  {reply, gen_read_index(alliance, ClanId)}.

add_alliance(ClanId1, ClanId2) ->
  gen_add_to_index(alliance, ClanId1, ClanId2),
  gen_add_to_index(alliance, ClanId2, ClanId1),
  {reply, ok}.

break_alliance(ClanId1, ClanId2) ->
  gen_remove_from_index(alliance, ClanId1, ClanId2),
  gen_remove_from_index(alliance, ClanId2, ClanId1),
  {reply, ok}.

list_alliance_requests(ClanId) ->
  {reply, gen_read_index(alliancereq, ClanId)}.

request_alliance(ClanId1, ClanId2) ->
  gen_add_to_index(alliancereq, ClanId1, ClanId2),
  {reply, ok}.

delete_alliance_request(ClanId1, ClanId2) ->
  gen_remove_from_index(alliancereq, ClanId1, ClanId2),
  {reply, ok}.

%%%-----------------------------------------------------------------------------
%%% INTERNAL
%%%-----------------------------------------------------------------------------

-spec gen_add_to_index(RecType :: atom(), Key1 :: integer()
                      , AddValues :: [ch_db:set_value()]) -> {reply, ok}.
gen_add_to_index(RecType, Key1, AddValues) when is_atom(RecType) ->
  {reply, ch_db:add_to_index({RecType, Key1}, AddValues)}.

-spec gen_remove_from_index(RecType :: atom(), Key1 :: integer()
                           , DeleteValues :: [ch_db:set_value()]) -> {reply, ok}.
gen_remove_from_index(RecType, Key1, DeleteValues) when is_atom(RecType) ->
  {reply, ch_db:remove_from_index({RecType, Key1}, DeleteValues)}.

-spec gen_read_index(RecType :: atom(), Key1 :: integer())
      -> {reply, list(ch_db:set_value())}.
gen_read_index(RecType, Key1) when is_atom(RecType) ->
  case riak_pool:with_worker(fun(Worker) ->
    ch_db:read_set_object(Worker, {RecType, Key1})
  end) of
    {ok, SetObject} -> riakc_set:value(SetObject);
    {error, _E}     -> []
  end.
