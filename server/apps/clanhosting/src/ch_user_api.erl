%%%-----------------------------------------------------------------------------
%%% @doc User functions
%%% @end
%%% Created : 15. Aug 2014 14:23
%%%-----------------------------------------------------------------------------
-module(ch_user_api).

%% API
-export([after_login/3,
  get_session/1, logged_out/1]).

%% @doc Возвращает информацию об игроке
-spec after_login(AccountId :: integer(), Token :: binary(), Lang :: binary())
      -> {reply, {bert, dict, proplists:proplist()}}.
after_login(AccountId, Token, Lang) ->
  QueryFun = fun() ->
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
          ch_lib:proplist_to_bert_dict(AccountInfo2, [])
        end,
  AccountInfo = ch_lib:memoize(ch_user_cache, AccountId, QueryFun),
  {reply, {bert, dict, AccountInfo}}.

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

