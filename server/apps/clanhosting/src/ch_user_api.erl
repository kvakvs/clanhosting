%%%-----------------------------------------------------------------------------
%%% @doc User functions
%%% @end
%%% Created : 15. Aug 2014 14:23
%%%-----------------------------------------------------------------------------
-module(ch_user_api).

%% API
-export([account_info/3,
  new_session/2,
  after_login/3,
  get_session/1, logged_out/1]).

%% @doc Возвращает информацию об игроке
-spec account_info(AccountId :: integer(), Token :: binary(), Lang :: binary())
      -> {reply, {bert, dict, proplists:proplist()}}.
account_info(AccountId, Token, Lang) ->
  Url = ch_http:format_url( ch_conf:wg_api_url("account", "info")
                          , [ {"access_token", binary_to_list(Token)}
                            , {"application_id", ch_conf:wg_app_id()}
                            , {"account_id", integer_to_list(AccountId)}
                            , {"language", binary_to_list(Lang)}
                            ]),
  {ok, AccountInfoList} = ch_lib:json_api_request(get, Url),
  %% Данные находятся в info["data"][account_id]
  AccountInfo1 = proplists:get_value(<<"data">>, AccountInfoList),
  IdBin = list_to_binary(integer_to_list(AccountId)),
  AccountInfo2 = proplists:get_value(IdBin, AccountInfo1),
  AccountInfo  = ch_lib:proplist_to_bert_dict(AccountInfo2, []),
  {reply, {bert, dict, AccountInfo}}.

%% @doc Создаёт новую сессию с айди и данными аккаунта. Для одновременного
%% вызова account_info и new_session можно использовать after_login(id)
-spec new_session(AccountId :: integer(), AccountInfo :: proplists:proplist())
      -> {reply, {bert, dict, proplists:proplist()}}.
new_session(AccountId, AccountInfo) ->
  ch_session_cache:new(AccountId, AccountInfo),
  {reply, AccountInfo}.

%% @doc Получить имеющуюся сессию или дать ошибку
-spec get_session(AccountId :: integer())
      -> {reply, {bert, dict, proplists:proplist()}} | {error, not_found}.
get_session(AccountId) ->
  case ch_session_cache:find(AccountId) of
    {ok, Ses}          -> {reply, Ses};
    {error, not_found} -> {error, not_found}
  end.

logged_out(AccountId) ->
  ch_session_cache:delete(AccountId),
  {reply, ok}.

%% @doc Вызывает цепочкой account_info затем new_session, для минимизации обмена
%% данными с вебом
-spec after_login(AccountId :: integer(), Token :: binary(), Lang :: binary())
      -> {reply, {bert, dict, proplists:proplist()}}.
after_login(AccountId, Token, Lang) ->
  {reply, AccountInfo} = account_info(AccountId, Token, Lang),
  {reply, _} = new_session(AccountId, AccountInfo),
  {reply, AccountInfo}.

%%%-----------------------------------------------------------------------------
%%% INTERNAL
%%%-----------------------------------------------------------------------------

