%%%-------------------------------------------------------------------
%%% @doc Риак интерфейс для данных как уникальных так и нумерованных, которые
%%% хранятся в bucket по номеру клана и могут иметь или не иметь id, в формате
%%% riak 2.0 map
%%% @end
%%% Created : 25. Aug 2014 20:49
%%%-------------------------------------------------------------------
-module(ch_db).

%% API
-export([update_map/4
        , read_map_object/2
        , make_key/2
        , make_id/0
        , update_set/5
        , read_set_object/2
        , delete_map/2
        , map_object_to_orddict/1
        , set_object_to_list/1
        , add_to_index/2
        , remove_from_index/2
        ]).
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
  AddFields = [{<<"updated_at">>, iso8601_now()} | AddFields0],
  Map0 = read_or_new_map(Worker, ObjectId),
  Map1 = lists:foldl(
    fun({UpdKey, UpdValue0}, MapAccum) ->
      UpdValue = libe_types:as_binary(UpdValue0),
      riakc_map:update({libe_types:as_binary(UpdKey), register},
                       fun(UpdReg) -> riakc_register:set(UpdValue, UpdReg) end,
                       MapAccum)
    end, Map0, AddFields),
  Map = lists:foldl(
    fun({UpdKey, UpdValue0}, MapAccum) ->
      UpdValue = libe_types:as_binary(UpdValue0),
      riakc_map:update({libe_types:as_binary(UpdKey), register},
        fun(UpdReg) -> riakc_register:set(UpdValue, UpdReg) end,
        MapAccum)
    end, Map1, EraseFields),
  riak_pool:update_type(Worker, Bucket, Key, riakc_map:to_op(Map)).

delete_map(Worker, ObjectId) ->
  {Bucket, Key} = make_key(<<"maps">>, ObjectId),
  riak_pool:delete(Worker, Bucket, Key).

%% @doc Использовать из riak_pool:with_worker(fun(W) -> ... end)
-spec read_map_object(Worker :: pid(), ObjectId :: type_and_key())
      -> {ok, riakc_map:crdt_map()} | {error, any()}.
read_map_object(Worker, ObjectId) ->
  {Bucket, Key} = make_key(<<"maps">>, ObjectId),
  case riak_pool:fetch_type(Worker, Bucket, Key) of
    {ok, MapObject}  -> {ok, MapObject};
    {error, E} -> {error, E}
  end.

%% @doc Достать из riakc_map обычный словарь
-spec map_object_to_orddict(riakc_map:crdt_map()) -> orddict:orddict().
map_object_to_orddict(MapObject) ->
  remove_register_from_keys(riakc_map:value(MapObject)).

%% @doc Достать из riakc_set список чего-нибудь
-spec set_object_to_list(riakc_set:riakc_set()) -> list().
set_object_to_list(SetObject) ->
  ordsets:to_list(riakc_set:value(SetObject)).

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
-spec read_set_object(Worker :: pid(), ObjectId :: type_and_key())
      -> {ok, riakc_set:riakc_set()} | {error, any()}.
read_set_object(Worker, ObjectId) ->
  {Bucket, Key} = make_key(<<"sets">>, ObjectId),
  case riak_pool:fetch_type(Worker, Bucket, Key) of
    {ok, SetObject} -> {ok, SetObject};
    {error, E} -> {error, E}
  end.

make_id() ->
  {ok, <<Id:128>>} = flake_server:id(),
  libe_hex:integer_to_base62(Id).

-spec add_to_index(TypeKey :: type_and_key(), AddIds :: [set_value()]) -> ok.
add_to_index(TypeKey, AddIds) ->
  riak_pool:with_worker(fun(Worker) ->
    update_set(Worker, TypeKey, existing, AddIds, [])
  end),
  ok.

-spec remove_from_index(TypeKey :: type_and_key(), DeleteIds :: [set_value()]) -> ok.
remove_from_index(TypeKey, DeleteIds) ->
  riak_pool:with_worker(fun(Worker) ->
    update_set(Worker, TypeKey, existing, [], DeleteIds)
  end),
  ok.

%%%-----------------------------------------------------------------------------
%%% INTERNAL
%%%-----------------------------------------------------------------------------

%% @private
%% @doc Returns new or possibly existing set value from database
read_or_new_set(_Worker, _ObjectId, new) ->
  riakc_set:new();
read_or_new_set(Worker, ObjectId, existing) ->
  case read_set_object(Worker, ObjectId) of
    {ok, S}    -> S;
    {error, _} -> riakc_set:new()
  end.

%% @private
%% @doc Returns new or possibly existing map from database
read_or_new_map(Worker, ObjectId) ->
  case read_map_object(Worker, ObjectId) of
    {ok, MapObj} -> MapObj;
    {error, _}   ->
      map_reg_set(riakc_map:new(), <<"created_at">>, iso8601_now())
  end.

%% @private
%% @doc Updates riakc_map and sets Key to Value
map_reg_set(Map, Key, Value) when is_binary(Key) ->
  riakc_map:update( {Key, register}
                  , fun(UpdReg) -> riakc_register:set(Value, UpdReg) end
                  , Map).

%% @private
%% @doc Formats current datetime as &lt;&lt;"2014-09-13T14:37:48+0000"&gt;&gt;
iso8601_now() ->
  {ok, Bin} = tempo:format(iso8601, {now, os:timestamp()}),
  Bin.

%% @private
%% @doc Unwraps elements of [{{K, register}, V}] making {{K, V}]
remove_register_from_keys(Props) ->
  [{K, V} || {{K, _Register}, V} <- Props].
