%%%-----------------------------------------------------------------------------
%%% @doc Thread post functions
%%% @end
%%% Created : 12. Sep 2014
%%%-----------------------------------------------------------------------------
-module(ch_post_api).

%% Threads API
-export([add_post/3, delete_post/3]).
%% Database API
-export([read_one/3, delete_one/3, update_one/4,
         add_to_index/3, read_index/2]).

-spec add_post(ClanId :: integer(), ThreadId :: binary(),
    Fields :: dict()) -> {reply, ok}.
add_post(ClanId, ThreadId, Fields) ->
  PostId = ch_db:make_id(),
  {reply, ok} = update_one(ClanId, ThreadId, PostId, Fields),
  {reply, ok} = add_to_index(ClanId, ThreadId, [PostId]),
  {reply, PostId}.

delete_post(ClanId, ThreadId, PostId) ->
  {reply, ok} = remove_from_index(ClanId, ThreadId, [PostId]),
  {reply, ok} = delete_one(ClanId, ThreadId, PostId).

-spec add_to_index(_ClanId :: integer(), ThreadId :: binary(),
    AddIds :: [ch_db:set_value()]) -> {reply, ok}.
add_to_index(_ClanId, ThreadId, AddIds) ->
  {reply, ch_db:add_to_index({post, ThreadId}, AddIds)}.

-spec remove_from_index(_ClanId :: integer(), ThreadId :: binary(),
    DeleteIds :: [ch_db:set_value()]) -> {reply, ok}.
remove_from_index(_ClanId, ThreadId, DeleteIds) ->
  {reply, ch_db:remove_from_index({post, ThreadId}, DeleteIds)}.

-spec update_one(_ClanId :: integer(), ThreadId :: binary(),
    PostId :: binary(), Fields :: ch_db:map_value()) -> {reply, ok}.
update_one(_ClanId, ThreadId, PostId, Fields0) ->
  Fields = dict:to_list(Fields0),
  riak_pool:with_worker(fun(Worker) ->
    ch_db:update_map(Worker, {post, ThreadId, PostId}, Fields, [])
  end),
  {reply, ok}.

-spec read_one(_ClanId :: integer(), ThreadId :: binary(), PostId :: binary())
      -> {reply, {bert, dict, proplists:proplist()}}.
read_one(_ClanId, ThreadId, PostId) ->
  case riak_pool:with_worker(fun(Worker) ->
    ch_db:read_map_object(Worker, {post, ThreadId, PostId})
  end) of
    {ok, MapObject} ->
      Map = ch_db:map_object_to_orddict(MapObject),
      {reply, {bert, dict, Map}};
    {error, _E} -> {reply, {bert, nil}}
  end.

-spec delete_one(_ClanId :: integer(), ThreadId :: binary(), PostId :: binary())
      -> {reply, ok}.
delete_one(_ClanId, ThreadId, PostId) ->
  case riak_pool:with_worker(fun(Worker) ->
                           ch_db:delete_map(Worker, {post, ThreadId, PostId})
                         end) of
    ok          -> {reply, ok};
    {error, _E} -> {reply, {bert, nil}}
  end.

-spec read_index(ClanId :: integer(), ThreadId :: binary())
      -> {reply, {bert, dict, proplists:proplist()}}.
read_index(_ClanId, ThreadId) ->
  case riak_pool:with_worker(fun(Worker) ->
    ch_db:read_set_object(Worker, {post, ThreadId})
  end) of
    {ok, SetObject} -> {reply, riakc_set:value(SetObject)};
    {error, _E}     -> {reply, {bert, nil}}
  end.

%%%-----------------------------------------------------------------------------
%%% INTERNAL
%%%-----------------------------------------------------------------------------

