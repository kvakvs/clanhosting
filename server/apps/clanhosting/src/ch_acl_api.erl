%%%-----------------------------------------------------------------------------
%%% @doc Access control for everything site/clan/forum/whatever
%%% @end
%%% Created : 30. Aug 2014
%%%-----------------------------------------------------------------------------
-module(ch_acl_api).

%% ACL API
-export([grant/3, revoke/3, has_access/3, read_for_user/2]).

-spec grant(ClanId :: integer(), AclName :: binary(), UserId :: integer())
      -> {reply, ok}.
grant(ClanId, AclName, UserId) ->
  riak_pool:with_worker(fun(Worker) ->
    ch_db:update_set(Worker, {acl, ClanId, UserId}, existing, [AclName], [])
  end),
  {reply, ok}.

-spec revoke(ClanId :: integer(), AclName :: binary(), UserId :: integer())
      -> {reply, ok}.
revoke(ClanId, AclName, UserId) ->
  riak_pool:with_worker(fun(Worker) ->
    ch_db:update_set(Worker, {acl, ClanId, UserId}, existing, [], [AclName])
  end),
  {reply, ok}.

-spec has_access(ClanId :: integer(), AclName :: binary(), UserId :: integer())
      -> {reply, integer()}.
has_access(ClanId, AclName, UserId) ->
  case riak_pool:with_worker(fun(Worker) ->
    ch_db:read_set_object(Worker, {acl, ClanId, UserId})
  end) of
    {ok, SetObject} ->
      case riakc_set:is_element(AclName, SetObject) of
        true -> {reply, 1};
        false -> {reply, 0}
      end;
    {error, _E} -> {reply, 0}
  end.

-spec read_for_user(ClanId :: integer(), UserId :: integer())
      -> {reply, {bert, dict, proplists:proplist()}}.
read_for_user(ClanId, UserId) ->
  case riak_pool:with_worker(fun(Worker) ->
    ch_db:read_set_object(Worker, {acl, ClanId, UserId})
  end) of
    {ok, SetObject} -> {reply, ch_db:set_object_to_list(SetObject)};
    {error, _E}     -> {reply, {bert, nil}}
  end.

%%%-----------------------------------------------------------------------------
%%% INTERNAL
%%%-----------------------------------------------------------------------------

