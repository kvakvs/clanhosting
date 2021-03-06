%%%-------------------------------------------------------------------
%%% @doc Выполнить запрос к API и раскодировать результат
%%% @end
%%% Created : 22. Aug 2014 20:18
%%%-------------------------------------------------------------------
-module(ch_lib).

%% API
-export([json_api_request/2, proplist_to_bert_dict/2, memoize/3]).

%% @private
%% Url - iolist with Url, flattened here, Method - "GET"
-spec json_api_request(Method :: string(), Url :: iolist())
      -> {ok, {struct, proplists:proplist()}} | {error, any()}.
json_api_request(Method, Url) ->
%%   case ch_http:Method(Url, [{timeout, 15000}]) of
%%     {ok, Result} ->
%%       Response = mochijson2:decode(ch_http:body(Result), [{format, proplist}]),
  case lhttpc:request(lists:flatten(Url), Method, [], 15000) of
    {ok, {{_StatusCode, _ReasonPhrase}, _Hdrs, ResponseBody}} ->
      Response = mochijson2:decode(ResponseBody, [{format, proplist}]),
      {ok, Response};
    {error, Reason} ->
      {error, Reason}
  end.

%% @doc Gets K element from proplist Prop, puts it back as {bert, dict, [...]}
proplist_to_bert_dict([], A) -> A;
proplist_to_bert_dict([{K, V0 = [{_,_}|_]} | Prop0], Accum) ->
  V = proplist_to_bert_dict(V0, []),
  proplist_to_bert_dict(Prop0, [{K, {bert, dict, V}} | Accum]);
proplist_to_bert_dict([{K, V} | Prop0], Accum) ->
  proplist_to_bert_dict(Prop0, [{K, V} | Accum]).

%% @doc Query cache:get or (if not found) run Fun() and memoize result
memoize(Cache, K, Fun) ->
  case cache:get(Cache, K) of
    undefined ->
      Value = Fun(),
      cache:put(Cache, K, Value),
      Value;
    Existing -> Existing
  end.