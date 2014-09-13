%%%-----------------------------------------------------------------------------
%%% @doc Thread functions
%%% @end
%%% Created : 10. Sep 2014
%%%-----------------------------------------------------------------------------
-module(ch_thread_api).

%% Threads API
-export([add_thread/3, delete_thread/3]).
%% Database API
-export([read_one/3, delete_one/3, update_one/4,
         add_to_index/3, read_index/2]).

-spec add_thread(ClanId :: integer(), ForumId :: binary(),
    Fields :: dict()) -> {reply, ok}.
add_thread(ClanId, ForumId, Fields) ->
  ThreadId = ch_db:make_id(),
  {reply, ok} = update_one(ClanId, ForumId, ThreadId, Fields),
  {reply, ok} = add_to_index(ClanId, ForumId, [ThreadId]),
  {reply, ThreadId}.

delete_thread(ClanId, ForumId, ThreadId) ->
  {reply, ok} = remove_from_index(ClanId, ForumId, [ThreadId]),
  {reply, ok} = delete_one(ClanId, ForumId, ThreadId),
  case ch_post_api:read_index(ClanId, ThreadId) of
    {reply, {bert, nil}} -> ok;
    {reply, Posts} ->
      lists:foreach(fun(P) ->
          ch_post_api:delete_post(ClanId, ThreadId, P)
        end,
        Posts)
  end,
  {reply, ok}.

-spec add_to_index(_ClanId :: integer(), ForumId :: binary(),
    AddIds :: [ch_db:set_value()]) -> {reply, ok}.
add_to_index(_ClanId, ForumId, AddIds) ->
  {reply, ch_db:add_to_index({thread, ForumId}, AddIds)}.

-spec remove_from_index(_ClanId :: integer(), ForumId :: binary(),
    DeleteIds :: [ch_db:set_value()]) -> {reply, ok}.
remove_from_index(_ClanId, ForumId, DeleteIds) ->
  {reply, ch_db:remove_from_index({thread, ForumId}, DeleteIds)}.

-spec update_one(_ClanId :: integer(), ForumId :: binary(),
    ThreadId :: binary(), Fields :: ch_db:map_value()) -> {reply, ok}.
update_one(_ClanId, ForumId, ThreadId, Fields0) ->
  Fields = dict:to_list(Fields0),
  riak_pool:with_worker(fun(Worker) ->
    ch_db:update_map(Worker, {thread, ForumId, ThreadId}, Fields, [])
  end),
  {reply, ok}.

-spec read_one(_ClanId :: integer(), ForumId :: binary(), ThreadId :: binary())
      -> {reply, {bert, dict, proplists:proplist()}}.
read_one(_ClanId, ForumId, ThreadId) ->
  case riak_pool:with_worker(fun(Worker) ->
    ch_db:read_map_object(Worker, {thread, ForumId, ThreadId})
  end) of
    {ok, MapObject} ->
      Map = ch_db:map_object_to_orddict(MapObject),
      {reply, {bert, dict, Map}};
    {error, _E} -> {reply, {bert, nil}}
  end.

-spec delete_one(_ClanId :: integer(), ForumId :: binary(),
    ThreadId :: binary()) -> {reply, ok}.
delete_one(_ClanId, ForumId, ThreadId) ->
  case riak_pool:with_worker(fun(Worker) ->
                           ch_db:delete_map(Worker, {thread, ForumId, ThreadId})
                         end) of
    ok          -> {reply, ok};
    {error, _E} -> {reply, {bert, nil}}
  end.

-spec read_index(ClanId :: integer(), ForumId :: binary())
      -> {reply, {bert, dict, proplists:proplist()}}.
read_index(_ClanId, ForumId) ->
  case riak_pool:with_worker(fun(Worker) ->
    ch_db:read_set_object(Worker, {thread, ForumId})
  end) of
    {ok, SetObject} -> {reply, riakc_set:value(SetObject)};
    {error, _E}     -> {reply, {bert, nil}}
  end.

%%%-----------------------------------------------------------------------------
%%% INTERNAL
%%%-----------------------------------------------------------------------------

