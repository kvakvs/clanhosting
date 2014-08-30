%%%-----------------------------------------------------------------------------
%%% @doc Access control for everything site/clan/forum/whatever
%%% @end
%%% Created : 30. Aug 2014
%%%-----------------------------------------------------------------------------
-module(ch_acl_api).

%% ACL API
-export([grant/3, revoke/3, has_access/3]).

-spec grant(ClanId :: integer(), AclName :: binary(), UserId :: integer())
      -> {reply, ok}.
grant(ClanId, AclName, UserId) ->
  riak_pool:with_worker(fun(Worker) ->
    ch_db:update_set(Worker, {acl, ClanId, AclName}, existing, [UserId], [])
  end),
  {reply, ok}.

-spec revoke(ClanId :: integer(), AclName :: binary(), UserId :: integer())
      -> {reply, ok}.
revoke(ClanId, AclName, UserId) ->
  riak_pool:with_worker(fun(Worker) ->
    ch_db:update_set(Worker, {acl, ClanId, AclName}, existing, [], [UserId])
  end),
  {reply, ok}.

-spec has_access(ClanId :: integer(), AclName :: binary(), UserId :: integer())
      -> {reply, integer()}.
has_access(ClanId, AclName, UserId) ->
  case riak_pool:with_worker(fun(Worker) ->
    ch_db:read_set_object(Worker, {acl, ClanId, AclName})
  end) of
    {ok, SetObject} ->
      case riakc_set:is_element(UserId, SetObject) of
        true -> {reply, 1};
        false -> {reply, 0}
      end;
    {error, _E} -> {reply, 0}
  end.

%%%-----------------------------------------------------------------------------
%%% INTERNAL
%%%-----------------------------------------------------------------------------

