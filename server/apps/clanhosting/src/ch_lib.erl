%%%-------------------------------------------------------------------
%%% @doc Выполнить запрос к API и раскодировать результат
%%% @end
%%% Created : 22. Aug 2014 20:18
%%%-------------------------------------------------------------------
-module(ch_lib).

%% API
-export([json_api_request/2]).

%% @private
%% Url - iolist with Url, flattened here, Method - "GET"
-spec json_api_request(Method :: string(), Url :: iolist())
      -> {ok, {struct, proplists:proplist()}} | {error, any()}.
json_api_request(Method, Url) ->
  case lhttpc:request(lists:flatten(Url), Method, [], 15000) of
    {ok, {{_StatusCode, _ReasonPhrase}, _Hdrs, ResponseBody}} ->
      Response = mochijson2:decode(ResponseBody, [{format, proplist}]),
%%       lager:debug("resp: ~p", [Response]),
      {ok, Response};
    {error, Reason} ->
      {error, Reason}
  end.
