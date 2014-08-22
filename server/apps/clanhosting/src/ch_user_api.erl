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
        get_session/1]).

%% @doc Возвращает информацию об игроке
%% IN: context[token], context[account_id], context[lang] :: string()
-spec account_info(AccountId :: integer(), Token :: binary(), Lang :: binary())
      -> {reply, proplists:proplist()}.
account_info(AccountId, Token, Lang) ->
%%   {ok, Session} = ch_session:find(AccountId),
%%   Token     = orddict:fetch(token, Session),
%%   AccountId = orddict:fetch(account_id, Session),
%%   Lang      = orddict:fetch(lang, Session),
  Url = [ch_conf:wg_api_url("account", "info"),
        "?access_token=", binary_to_list(Token),
        "&application_id=", ch_conf:wg_app_id(),
        "&account_id=", integer_to_list(AccountId),
        "&language=", binary_to_list(Lang)
        ],
  {ok, AccountInfoList} = do_request("GET", Url),
  %% Данные находятся в info["data"][account_id]
  AccountInfo1 = proplists:get_value(<<"data">>, AccountInfoList),
  IdBin = list_to_binary(integer_to_list(AccountId)),
  AccountInfo  = proplists:get_value(IdBin, AccountInfo1),
  {reply, {bert, dict, AccountInfo}}.

%% @doc Создаёт новую сессию с айди и данными аккаунта. Для одновременного
%% вызова account_info и new_session можно использовать after_login(id)
-spec new_session(AccountId :: integer(), AccountInfo :: proplists:proplist())
      -> {reply, proplists:proplist()}.
new_session(AccountId, AccountInfo) ->
  ch_session:new(AccountId, AccountInfo),
  {reply, AccountInfo}.

%% @doc Получить имеющуюся сессию или дать ошибку
-spec get_session(AccountId :: integer())
      -> {reply, proplists:proplist()} | {error, not_found}.
get_session(AccountId) ->
  case ch_session:find(AccountId) of
    {ok, Ses}          -> {reply, Ses};
    {error, not_found} -> {error, not_found}
  end.

%% @doc Вызывает цепочкой account_info затем new_session, для минимизации обмена
%% данными с вебом
-spec after_login(AccountId :: integer(), Token :: binary(), Lang :: binary())
      -> {reply, proplists:proplist()}.
after_login(AccountId, Token, Lang) ->
  {reply, AccountInfo} = account_info(AccountId, Token, Lang),
  {reply, _} = new_session(AccountId, AccountInfo),
  {reply, AccountInfo}.

%%%-----------------------------------------------------------------------------
%%% INTERNAL
%%%-----------------------------------------------------------------------------

%% @private
%% Url - iolist with Url, flattened here, Method - "GET"
-spec do_request(Method :: string(), Url :: iolist())
      -> {ok, {struct, proplists:proplist()}} | {error, any()}.
do_request(Method, Url) ->
  case lhttpc:request(lists:flatten(Url), Method, [], 15000) of
    {ok, {{_StatusCode, _ReasonPhrase}, _Hdrs, ResponseBody}} ->
      Response = mochijson2:decode(ResponseBody, [{format, proplist}]),
%%       lager:debug("resp: ~p", [Response]),
      {ok, Response};
    {error, Reason} ->
      {error, Reason}
  end.
