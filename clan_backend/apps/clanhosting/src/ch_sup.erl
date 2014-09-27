-module(ch_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
%% child(I, Type) -> {I, {I, start_link, []}, permanent, 5000, Type, [I]}.
cache_worker(Id, Type, Module, Args) ->
  {Id, {Module, start_link, [Id] ++ Args}, permanent, 5000, Type, [Id]}.

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
  {ok, { {one_for_one, 5, 10},
    [ cache_worker(ch_session_cache, worker, cache, [[{n, 6}, {ttl, 3600}]])
    , cache_worker(ch_clan_cache, worker, cache, [[{n, 6}, {ttl, 3600}]])
    , cache_worker(ch_user_cache, worker, cache, [[{n, 6}, {ttl, 3600}]])
    , cache_worker(ch_clan_search_cache, worker, cache, [[{n, 4}, {ttl, 300}]])
    ]}
  }.
