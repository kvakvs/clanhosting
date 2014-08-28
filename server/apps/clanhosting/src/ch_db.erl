%%%-------------------------------------------------------------------
%%% @doc Риак интерфейс для данных как уникальных так и нумерованных, которые
%%% хранятся в bucket по номеру клана и могут иметь или не иметь id, в формате
%%% riak 2.0 map
%%% @end
%%% Created : 25. Aug 2014 20:49
%%%-------------------------------------------------------------------
-module(ch_db).

%% API
-export([update_map/3, read_map/2,
        make_key/2,
        update_set/3, read_set/2]).
-type type_and_key() :: {atom(), integer(), integer()} | {atom(), integer()}.

-spec make_key(RiakType :: binary(),
               ObjectId :: type_and_key()) -> {binary(), binary()}.
make_key(RiakType, {TableType, ClanId}) ->
  %% Создаёт сложный ключ для сайта (без айди, один для бакета)
  ClanIdBin = libe_types:as_binary(ClanId),
  UniqueKey = libe_types:as_binary(TableType),
  {{RiakType, <<"clan-", ClanIdBin/binary>>}, UniqueKey};
make_key(RiakType, {TableType, ClanId, Id}) ->
  %% Создаёт сложный ключ для сайта (с айди, уникальный внутри бакета)
  ClanIdBin = libe_types:as_binary(ClanId),
  TypeKey   = libe_types:as_binary(TableType),
  IdBin     = libe_types:as_binary(Id),
  {{RiakType, <<"clan-", ClanIdBin/binary>>},
    <<TypeKey/binary, "-", IdBin/binary>>}.

%% @doc Использовать из riak_pool:with_worker(fun(W) -> ... end)
update_map(Worker, ObjectId, Fields) ->
  {Bucket, Key} = make_key(<<"maps">>, ObjectId),
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
-spec read_map(Worker :: pid(), ObjectId :: type_and_key()) -> {ok|error, any()}.
read_map(Worker, ObjectId) ->
  {Bucket, Key} = make_key(<<"maps">>, ObjectId),
  case riak_pool:fetch_type(Worker, Bucket, Key) of
    {ok, Obj} ->
      {ok, riakc_map:value(Obj)};
    {error, E} ->
      {error, E}
  end.

%% @doc Использовать из riak_pool:with_worker(fun(W) -> ... end)
update_set(Worker, ObjectId, Fields) ->
  {Bucket, Key} = make_key(<<"sets">>, ObjectId),
  Set = lists:foldl(
    fun(UpdValue0, SetAccum) ->
      riakc_set:add_element(libe_types:as_binary(UpdValue0), SetAccum)
    end,
    riakc_set:new(),
    Fields),
  riak_pool:update_type(Worker, Bucket, Key, riakc_set:to_op(Set)).

%% @doc Использовать из riak_pool:with_worker(fun(W) -> ... end)
-spec read_set(Worker :: pid(), ObjectId :: type_and_key()) -> {ok|error, any()}.
read_set(Worker, ObjectId) ->
  {Bucket, Key} = make_key(<<"sets">>, ObjectId),
  case riak_pool:fetch_type(Worker, Bucket, Key) of
    {ok, Obj} ->
      {ok, riakc_set:value(Obj)};
    {error, E} ->
      {error, E}
  end.
