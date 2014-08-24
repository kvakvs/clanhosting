%%%-----------------------------------------------------------------------------
%%% @doc Site functions
%%% @end
%%% Created : 15. Aug 2014 14:23
%%%-----------------------------------------------------------------------------
-module(ch_site_api).

%% API
-export([exists/1, new/2, read/1]).

-spec exists(ClanId :: integer()) -> {reply, {bert, boolean()}}.
exists(ClanId) ->
  {Bucket, Key} = bk_for_site(ClanId),
  case riak_pool:with_worker(fun(Worker) -> db_read(Worker, Bucket, Key) end) of
    {ok, _Value} -> {reply, 1}; %{bert, true}};
    {error, _E}  -> {reply, 0}  %{bert, false}}
  end.

-spec new(ClanId :: integer(), Fields :: proplists:proplist()) -> {reply, ok}.
new(ClanId, Fields0) ->
  {Bucket, Key} = bk_for_site(ClanId),
  Fields = dict:to_list(Fields0),
  riak_pool:with_worker(fun(Worker) -> db_write(Worker, Bucket, Key, Fields) end),
  {reply, ok}.

-spec read(ClanId :: integer()) -> {reply, {bert, dict, proplists:proplist()}}.
read(ClanId) ->
  {Bucket, Key} = bk_for_site(ClanId),
  case riak_pool:with_worker(fun(Worker) -> db_read(Worker, Bucket, Key) end) of
    {ok, Value} -> {reply, {bert, dict, Value}};
    {error, _E}  -> {error, not_found}
  end.

%%%-----------------------------------------------------------------------------
%%% INTERNAL
%%%-----------------------------------------------------------------------------
bk_for_site(ClanId) when is_integer(ClanId) ->
  ClanIdBin = list_to_binary(integer_to_list(ClanId)),
  {<<"ch-sites">>, <<"site-", ClanIdBin/binary>>}.

db_read(Worker, Bucket, Key) ->
  case riak_pool:get(Worker, Bucket, Key) of
    {ok, Obj} ->
      Data = riakc_obj:get_value(Obj),
      {ok, binary_to_term(Data)};
    {error, E} ->
      {error, E}
  end.

db_write(Worker, Bucket, Key, Data) ->
  Obj = riakc_obj:new(Bucket, Key, erlang:term_to_binary(Data)),
  riak_pool:put(Worker, Obj).
