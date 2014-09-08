%%%-----------------------------------------------------------------------------
%%% @doc Forum functions
%%% @end
%%% Created : 07. Sep 2014
%%%-----------------------------------------------------------------------------
-module(ch_forum_api).

%% Posts API
-export([add_forum/2, delete_forum/2]).
%% Database API
-export([read_one/2, delete_one/2, update_one/3,
         add_to_index/2, read_index/1]).

-spec add_forum(ClanId :: integer(), Fields :: dict()) -> {reply, ok}.
add_forum(ClanId, Fields) ->
  ForumId = ch_db:make_id(),
  {reply, ok} = update_one(ClanId, ForumId, Fields),
  {reply, ok} = add_to_index(ClanId, [ForumId]).

delete_forum(ClanId, ForumId) ->
  {reply, ok} = remove_from_index(ClanId, [ForumId]),
  {reply, ok} = delete_one(ClanId, ForumId).

-spec add_to_index(ClanId :: integer(), AddIds :: [ch_db:set_value()])
      -> {reply, ok}.
add_to_index(ClanId, AddIds) ->
  {reply, ch_db:add_to_index({forum, ClanId}, AddIds)}.

-spec remove_from_index(ClanId :: integer(), DeleteIds :: [ch_db:set_value()])
      -> {reply, ok}.
remove_from_index(ClanId, DeleteIds) ->
  {reply, ch_db:remove_from_index({forum, ClanId}, DeleteIds)}.

-spec update_one(ClanId :: integer(), Id :: binary(),
                 Fields :: ch_db:map_value()) -> {reply, ok}.
update_one(ClanId, Id, Fields0) ->
  Fields = dict:to_list(Fields0),
  riak_pool:with_worker(fun(Worker) ->
    ch_db:update_map(Worker, {forum, ClanId, Id}, Fields, [])
  end),
  {reply, ok}.

-spec read_one(ClanId :: integer(), Id :: binary())
      -> {reply, {bert, dict, proplists:proplist()}}.
read_one(ClanId, Id) ->
  case riak_pool:with_worker(fun(Worker) ->
    ch_db:read_map_object(Worker, {forum, ClanId, Id})
  end) of
    {ok, MapObject} ->
      Map = ch_db:map_object_to_orddict(MapObject),
      {reply, {bert, dict, Map}};
    {error, _E} -> {reply, {bert, nil}}
  end.

-spec delete_one(ClanId :: integer(), Id :: binary()) -> {reply, ok}.
delete_one(ClanId, Id) ->
  case riak_pool:with_worker(fun(Worker) ->
                               ch_db:delete_map(Worker, {forum, ClanId, Id})
                             end) of
    ok          -> {reply, ok};
    {error, _E} -> {reply, {bert, nil}}
  end.

-spec read_index(ClanId :: integer())
      -> {reply, {bert, dict, proplists:proplist()}}.
read_index(ClanId) ->
  case riak_pool:with_worker(fun(Worker) ->
    ch_db:read_set_object(Worker, {forum, ClanId})
  end) of
    {ok, SetObject} -> {reply, riakc_set:value(SetObject)};
    {error, _E}     -> {reply, {bert, nil}}
  end.

%%%-----------------------------------------------------------------------------
%%% INTERNAL
%%%-----------------------------------------------------------------------------

