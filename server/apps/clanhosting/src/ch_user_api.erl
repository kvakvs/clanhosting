%%%-----------------------------------------------------------------------------
%%% @doc User functions
%%% @end
%%% Created : 15. Aug 2014 14:23
%%%-----------------------------------------------------------------------------
-module(ch_user_api).

%% API
-export([account_info/1]).

%% @doc Возвращает информацию об игроке
%% IN: context[token], context[account_id], context[lang] :: string()
-spec account_info(AccountId :: integer()) -> orddict:orddict().
account_info(AccountId) ->
  {ok, Session} = ch_session:find(AccountId),
  Token     = orddict:fetch(token, Session),
  AccountId = orddict:fetch(account_id, Session),
  Lang      = orddict:fetch(lang, Session),
  Url = [ch_conf:wg_api_url("account", "info"),
        "?access_token=", Token,
        "&application_id=", ch_conf:wg_app_id(),
        "&account_id=", AccountId,
        "&language=", Lang
        ],
  do_request("GET", Url).

%% @private
%% Url - iolist with Url, flattened here, Method - "GET"
-spec do_request(Method :: string(), Url :: iolist())
      -> {ok, {struct, proplists:proplist()}} | {error, any()}.
do_request(Method, Url) ->
  case lhttpc:request(lists:flatten(Url), Method, [], 15000) of
    {ok, {{_StatusCode, _ReasonPhrase}, _Hdrs, ResponseBody}} ->
      {ok, mochijson2:decode(ResponseBody)};
    {error, Reason} ->
      {error, Reason}
  end.
