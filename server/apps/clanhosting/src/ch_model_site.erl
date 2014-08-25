%%%-------------------------------------------------------------------
%%% @doc Модель данных для сайта
%%% @end
%%% Created : 25. Aug 2014 20:49
%%%-------------------------------------------------------------------
-module(ch_model_site).

%% API
-export([make_key/1, update/3, read/2]).

%% @doc Создаёт сложный ключ для сайта
make_key(ClanId) when is_integer(ClanId) ->
  ClanIdBin = list_to_binary(integer_to_list(ClanId)),
  {{<<"maps">>, <<"clansite-", ClanIdBin/binary>>},
    <<"site">>}.

%% @doc Использовать из riak_pool:with_worker(fun(W) -> ... end)
update(Worker, ClanId, Fields) ->
  {Bucket, Key} = make_key(ClanId),
  Map = lists:foldl(
    fun({UpdKey, UpdValue0}, MapAccum) ->
      UpdValue = libe_types:as_binary(UpdValue0),
      riakc_map:update({libe_types:as_binary(UpdKey), register},
                       fun(UpdReg) -> riakc_register:set(UpdValue, UpdReg) end,
                       MapAccum)
    end,
    riakc_map:new(),
    Fields),
  riak_pool:update_type(Worker, Bucket, Key, riakc_map:to_op(Map)).

%% @doc Использовать из riak_pool:with_worker(fun(W) -> ... end)
read(Worker, ClanId) ->
  {Bucket, Key} = make_key(ClanId),
  case riak_pool:fetch_type(Worker, Bucket, Key) of
    {ok, Obj} ->
      {ok, riakc_map:value(Obj)};
    {error, E} ->
      {error, E}
  end.
