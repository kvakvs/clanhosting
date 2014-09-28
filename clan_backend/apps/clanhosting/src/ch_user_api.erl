%%%-----------------------------------------------------------------------------
%%% @doc User functions
%%% @end
%%% Created : 15. Aug 2014 14:23
%%%-----------------------------------------------------------------------------
-module(ch_user_api).

%% API
-export([query_account_info/3
        , get_session/1
        , logged_out/1
        , get_account_info/1
        , get_username/1
        ]).

%% @doc Возвращает информацию об игроке
-spec query_account_info(AccountId :: integer(), Token :: binary(),
    Lang :: binary()) -> {reply, {bert, dict, proplists:proplist()}}.
query_account_info(AccountId, Token, Lang) ->
  QueryFun = fun() ->
          Url = ch_http:format_url( ch_conf:wg_api_url("account", "info")
                                  , [ {"access_token", binary_to_list(Token)}
                                    , {"application_id", ch_conf:wg_app_id()}
                                    , {"account_id", integer_to_list(AccountId)}
                                    , {"language", binary_to_list(Lang)}
                                    ]),
          {ok, AccountInfoList} = ch_lib:json_api_request(get, Url),
          %% Данные находятся в info["data"][account_id]
          case proplists:get_value(<<"data">>, AccountInfoList) of
            undefined ->
              {error, <<"not_found">>};
            AccountInfo1 ->
              IdBin = list_to_binary(integer_to_list(AccountId)),
              AccountInfo2 = proplists:get_value(IdBin, AccountInfo1),
              ch_lib:proplist_to_bert_dict(AccountInfo2, [])
          end
        end,
  case ch_lib:memoize(ch_user_cache, AccountId, QueryFun) of
    {error, _} = E -> E;
    AccountInfo    -> {reply, {bert, dict, AccountInfo}}
  end.

%% @doc Возвращает информацию об игроке (без токена и языка)
-spec get_account_info(AccountId :: integer()) ->
  {reply, {bert, dict, proplists:proplist()}}.
get_account_info(AccountId) ->
  QueryFun = fun() ->
    Url = ch_http:format_url( ch_conf:wg_api_url("account", "info")
      , [ {"application_id", ch_conf:wg_app_id()}
        , {"account_id", integer_to_list(AccountId)}
        ]),
    {ok, AccountInfoList} = ch_lib:json_api_request(get, Url),
    %% Данные находятся в info["data"][account_id]
    case proplists:get_value(<<"data">>, AccountInfoList) of
      undefined ->
        {error, <<"not_found">>};
      AccountInfo1 ->
        IdBin = list_to_binary(integer_to_list(AccountId)),
        AccountInfo2 = proplists:get_value(IdBin, AccountInfo1),
        ch_lib:proplist_to_bert_dict(AccountInfo2, [])
    end
  end,
  case ch_lib:memoize(ch_user_cache, AccountId, QueryFun) of
    {error, _} = E    -> E;
    CachedAccountInfo -> {reply, {bert, dict, CachedAccountInfo}}
  end.

get_username(AccountId) ->
  QueryFun = fun() ->
      case get_account_info(AccountId) of
        {reply, {bert, dict, A}} ->
          {reply, proplists:get_value(<<"nickname">>, A)};
        {error, _} = E -> E
      end
    end,
  case ch_lib:memoize(ch_user_name_cache, AccountId, QueryFun) of
    {error, _}=E   -> E;
    CachedUsername -> {reply, CachedUsername}
  end.

%% @!doc Создаёт новую сессию с айди и данными аккаунта. Для одновременного
%% вызова account_info и new_session можно использовать after_login(id)
%% -spec new_session(AccountId :: integer(), AccountInfo :: proplists:proplist())
%%       -> {reply, {bert, dict, proplists:proplist()}}.
%% new_session(AccountId, AccountInfo) ->
%%   cache:put(ch_user_cache, AccountId, AccountInfo),
%%   {reply, AccountInfo}.

%% @doc Получить имеющуюся сессию или дать ошибку
-spec get_session(AccountId :: integer())
      -> {reply, {bert, dict, proplists:proplist()}} | {error, not_found}.
get_session(AccountId) ->
  case ch_session_cache:find(AccountId) of
    {ok, Ses}          -> {reply, Ses};
    {error, not_found} -> {error, not_found}
  end.

logged_out(AccountId) ->
  cache:delete(ch_user_cache, AccountId),
  {reply, ok}.

%%%-----------------------------------------------------------------------------
%%% INTERNAL
%%%-----------------------------------------------------------------------------

