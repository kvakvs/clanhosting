%%%-------------------------------------------------------------------
%%% @doc Риак интерфейс для данных как уникальных так и нумерованных, которые
%%% хранятся в bucket по номеру клана и могут иметь или не иметь id, в формате
%%% riak 2.0 map
%%% @end
%%% Created : 25. Aug 2014 20:49
%%%-------------------------------------------------------------------
-module(ch_db).

%% API
-export([update_map/4,
  read_map/2,
  make_key/2,
  make_id/0,
  update_set/5,
  read_set/2,
  delete_map/2]).

-type type_and_key() :: {atom(), integer(), binary()} | {atom(), integer()}.
-type set_value() :: [binary()].
-type map_value() :: dict().

-export_type([set_value/0, map_value/0]).

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
update_map(Worker, ObjectId, AddFields0, EraseFields) ->
  {Bucket, Key} = make_key(<<"maps">>, ObjectId),
  AddFields = [{<<"updated_at">>, format_now()} | AddFields0],
  Map0 = lists:foldl(
    fun({UpdKey, UpdValue0}, MapAccum) ->
      UpdValue = libe_types:as_binary(UpdValue0),
      riakc_map:update({libe_types:as_binary(UpdKey), register},
                       fun(UpdReg) -> riakc_register:set(UpdValue, UpdReg) end,
                       MapAccum)
    end, riakc_map:new(), AddFields),
  Map = lists:foldl(
    fun({UpdKey, UpdValue0}, MapAccum) ->
      UpdValue = libe_types:as_binary(UpdValue0),
      riakc_map:update({libe_types:as_binary(UpdKey), register},
        fun(UpdReg) -> riakc_register:set(UpdValue, UpdReg) end,
        MapAccum)
    end, Map0, EraseFields),
  riak_pool:update_type(Worker, Bucket, Key, riakc_map:to_op(Map)).

delete_map(Worker, ObjectId) ->
  {Bucket, Key} = make_key(<<"maps">>, ObjectId),
  riak_pool:delete(Worker, Bucket, Key).

%% @doc Использовать из riak_pool:with_worker(fun(W) -> ... end)
-spec read_map(Worker :: pid(), ObjectId :: type_and_key()) -> {ok|error, any()}.
read_map(Worker, ObjectId) ->
  {Bucket, Key} = make_key(<<"maps">>, ObjectId),
  case riak_pool:fetch_type(Worker, Bucket, Key) of
    {ok, Obj} ->
      {ok, remove_register_from_keys(riakc_map:value(Obj))};
    {error, E} ->
      {error, E}
  end.

%% @doc Использовать из riak_pool:with_worker(fun(W) -> ... end)
-spec update_set(Worker :: pid(), ObjectId :: type_and_key(),
    BasedOn :: new | existing, Add :: [set_value()], Delete :: [set_value()])
      -> any().
update_set(Worker, ObjectId, BasedOn, Add, Delete) ->
  {Bucket, Key} = make_key(<<"sets">>, ObjectId),
  Set0 = read_or_new_set(Worker, ObjectId, BasedOn),
  Set1 = lists:foldl(
    fun(UpdValue0, AddAccum) ->
      riakc_set:add_element(libe_types:as_binary(UpdValue0), AddAccum)
    end, Set0, Add),
  Set = lists:foldl(
    fun(UpdValue0, DelAccum) ->
      riakc_set:del_element(libe_types:as_binary(UpdValue0), DelAccum)
    end, Set1, Delete),
  riak_pool:update_type(Worker, Bucket, Key, riakc_set:to_op(Set)).

%% @doc Использовать из riak_pool:with_worker(fun(W) -> ... end)
-spec read_set(Worker :: pid(), ObjectId :: type_and_key()) -> {ok|error, any()}.
read_set(Worker, ObjectId) ->
  {Bucket, Key} = make_key(<<"sets">>, ObjectId),
  case riak_pool:fetch_type(Worker, Bucket, Key) of
    {ok, SetObject} -> {ok, SetObject};
    {error, E} -> {error, E}
  end.

make_id() ->
  {ok, Id} = flake_server:id(),
  libe_hex:bin_to_hex(Id).

%%%-----------------------------------------------------------------------------
%%% INTERNAL
%%%-----------------------------------------------------------------------------

%% @private
%% @doc Returns new or possibly existing set value from database
read_or_new_set(_Worker, _ObjectId, new) ->
  riakc_set:new();
read_or_new_set(Worker, ObjectId, existing) ->
  case read_set(Worker, ObjectId) of
    {ok, S}    -> S;
    {error, _} -> riakc_set:new()
  end.

%% @private
format_now() ->
  {ok, Bin} = tempo:format(iso8601, {now, os:timestamp()}),
  Bin.

%% @private
remove_register_from_keys(Props) ->
  [{K, V} || {{K, _Register}, V} <- Props].
