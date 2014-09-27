%%%-----------------------------------------------------------------------------
%%% @doc Public and personal news feed functions
%%% @end
%%% Created : 26. Aug 2014
%%%-----------------------------------------------------------------------------
-module(ch_newsfeed_api).

%% Posts API
-export([add_post/2, delete_post/2]).
%% Database API
-export([read_one/2, delete_one/2, update_one/3,
         add_to_index/2, read_index/1]).

-spec add_post(ClanId :: integer(), Fields :: dict()) -> {reply, ok}.
add_post(ClanId, Fields) ->
%%   {reply, {bert, dict, Index}} = read_index(ClanId),
  PostId = ch_db:make_id(),
  {reply, ok} = update_one(ClanId, PostId, Fields),
  {reply, ok} = add_to_index(ClanId, [PostId]).

delete_post(ClanId, PostId) ->
  {reply, ok} = remove_from_index(ClanId, [PostId]),
  {reply, ok} = delete_one(ClanId, PostId).

-spec add_to_index(ClanId :: integer(), AddIds :: [ch_db:set_value()])
      -> {reply, ok}.
add_to_index(ClanId, AddIds) ->
  {reply, ch_db:add_to_index({newsfeed, ClanId}, AddIds)}.

-spec remove_from_index(ClanId :: integer(), DeleteIds :: [ch_db:set_value()])
      -> {reply, ok}.
remove_from_index(ClanId, DeleteIds) ->
  {reply, ch_db:remove_from_index({newsfeed, ClanId}, DeleteIds)}.

-spec update_one(ClanId :: integer(), Id :: binary(),
                 Fields :: ch_db:map_value()) -> {reply, ok}.
update_one(ClanId, Id, Fields0) ->
  Fields = dict:to_list(Fields0),
  riak_pool:with_worker(fun(Worker) ->
    ch_db:update_map(Worker, {newsfeed, ClanId, Id}, Fields, [])
  end),
  {reply, ok}.

-spec read_one(ClanId :: integer(), Id :: binary())
      -> {reply, {bert, dict, proplists:proplist()}}.
read_one(ClanId, Id) ->
  case riak_pool:with_worker(fun(Worker) ->
    ch_db:read_map_object(Worker, {newsfeed, ClanId, Id})
  end) of
    {ok, MapObject} ->
      Map = ch_db:map_object_to_orddict(MapObject),
      {reply, {bert, dict, Map}};
    {error, _E} -> {reply, {bert, nil}}
  end.

-spec delete_one(ClanId :: integer(), Id :: binary()) -> {reply, ok}.
delete_one(ClanId, Id) ->
  case riak_pool:with_worker(fun(Worker) ->
                               ch_db:delete_map(Worker, {newsfeed, ClanId, Id})
                             end) of
    ok          -> {reply, ok};
    {error, _E} -> {reply, {bert, nil}}
  end.

%% TODO: Index can grow big. Force clean or shard.
-spec read_index(ClanId :: integer())
      -> {reply, {bert, dict, proplists:proplist()}}.
read_index(ClanId) ->
  case riak_pool:with_worker(fun(Worker) ->
    ch_db:read_set_object(Worker, {newsfeed, ClanId})
  end) of
    {ok, SetObject} -> {reply, riakc_set:value(SetObject)};
    {error, _E}     -> {reply, {bert, nil}}
  end.

%%%-----------------------------------------------------------------------------
%%% INTERNAL
%%%-----------------------------------------------------------------------------

