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
  case riak_pool:with_worker(fun(Worker) ->
                               ch_model_site:read(Worker, ClanId)
                             end) of
    {ok, _Value} -> {reply, 1}; %{bert, true}};
    {error, _E}  -> {reply, 0}  %{bert, false}}
  end.

-spec new(ClanId :: integer(), Fields :: proplists:proplist()) -> {reply, ok}.
new(ClanId, Fields0) ->
  Fields = dict:to_list(Fields0),
  riak_pool:with_worker(fun(Worker) ->
                          ch_model_site:create(Worker, ClanId, Fields)
                        end),
  {reply, ok}.

-spec read(ClanId :: integer()) -> {reply, {bert, dict, proplists:proplist()}}.
read(ClanId) ->
  case riak_pool:with_worker(fun(Worker) ->
                               ch_model_site:read(Worker, ClanId)
                             end) of
    {ok, Value} -> {reply, {bert, dict, Value}};
    {error, _E} -> {reply, {bert, nil}}
  end.

%%%-----------------------------------------------------------------------------
%%% INTERNAL
%%%-----------------------------------------------------------------------------

