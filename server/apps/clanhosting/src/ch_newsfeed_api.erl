%%%-----------------------------------------------------------------------------
%%% @doc Public and personal news feed functions
%%% @end
%%% Created : 26. Aug 2014
%%%-----------------------------------------------------------------------------
-module(ch_newsfeed_api).

%% API
-export([read_one/2, update_index/2, update_one/3, read_index/1]).

-spec update_index(ClanId :: integer(), Fields :: dict:dict()) -> {reply, ok}.
update_index(ClanId, Fields0) ->
  Fields = dict:to_list(Fields0),
  riak_pool:with_worker(fun(Worker) ->
                          ch_db:update_map(Worker, {newsfeed, ClanId}, Fields)
                        end),
  {reply, ok}.

-spec update_one(ClanId :: integer(), Id :: integer(),
                 Fields :: dict:dict()) -> {reply, ok}.
update_one(ClanId, Id, Fields0) ->
  Fields = dict:to_list(Fields0),
  riak_pool:with_worker(fun(Worker) ->
    ch_db:update_map(Worker, {newsfeed, ClanId, Id}, Fields)
  end),
  {reply, ok}.

-spec read_one(ClanId :: integer(), Id :: integer())
      -> {reply, {bert, dict, proplists:proplist()}}.
read_one(ClanId, Id) ->
  case riak_pool:with_worker(fun(Worker) ->
                               ch_db:read_map(Worker, {newsfeed, ClanId, Id})
                             end) of
    {ok, Value} -> {reply, {bert, dict, Value}};
    {error, _E} -> {reply, {bert, nil}}
  end.

-spec read_index(ClanId :: integer()) -> {reply, {bert, dict, proplists:proplist()}}.
read_index(ClanId) ->
  case riak_pool:with_worker(fun(Worker) ->
    ch_db:read_set(Worker, {newsfeed, ClanId})
  end) of
    {ok, Value} -> {reply, {bert, dict, Value}};
    {error, _E} -> {reply, {bert, nil}}
  end.

%%%-----------------------------------------------------------------------------
%%% INTERNAL
%%%-----------------------------------------------------------------------------

