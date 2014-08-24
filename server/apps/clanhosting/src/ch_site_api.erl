%%%-----------------------------------------------------------------------------
%%% @doc Site functions
%%% @end
%%% Created : 15. Aug 2014 14:23
%%%-----------------------------------------------------------------------------
-module(ch_site_api).

%% API
-export([exists/1]).

%% @doc Возвращает информацию об игроке из кэша или из базы данных Wargaming
-spec exists(ClanId :: integer()) -> {reply, {bert, boolean()}}.
exists(ClanId) ->
  {Bucket, Key} = bk_for_site(ClanId),
  %case riak_pool:with_worker(fun(Worker) -> db_read(Worker, Bucket, Key) end) of
  case riak_pool_auto:get(Bucket, Key) of
    {ok, _Value} -> {reply, 1}; %{bert, true}};
    {error, _E}  -> {reply, 0}  %{bert, false}}
  end.

%%%-----------------------------------------------------------------------------
%%% INTERNAL
%%%-----------------------------------------------------------------------------
bk_for_site(ClanId) when is_integer(ClanId) ->
  ClanIdBin = list_to_binary(integer_to_list(ClanId)),
  {<<"ch-sites">>, <<"site-", ClanIdBin/binary>>}.

%% db_read(Worker, Bucket, Key) ->
%%   case riak_pool:get(Worker, Bucket, Key) of
%%     {ok, Obj} ->
%%       Data = riakc_obj:get_value(Obj),
%%       {ok, binary_to_term(Data)};
%%     {error, E} ->
%%       {error, E}
%%   end.