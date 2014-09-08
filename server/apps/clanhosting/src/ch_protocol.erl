%%%-------------------------------------------------------------------
%%% @doc BERT RPC Protocol for Clan Hosting
%%% @end
%%% Created : 15. Aug 2014 10:01
%%%-------------------------------------------------------------------
-module(ch_protocol).

-behaviour(ranch_protocol).

-export([start_link/4,
         init/4]).

-define(TIMEOUT, 5000).

start_link(Ref, Socket, Transport, Opts) ->
  Pid = spawn_link(?MODULE, init, [Ref, Socket, Transport, Opts]),
  {ok, Pid}.

init(Ref, Socket, Transport, _Opts = []) ->
  ok = ranch:accept_ack(Ref),
  loop(Socket, Transport).

loop(Socket, Transport) ->
  case Transport:recv(Socket, 4, ?TIMEOUT) of
    {ok, <<Length:32/big>>} ->
      case Transport:recv(Socket, Length, ?TIMEOUT) of
        {ok, Data} ->
          Reply = decode_packet(Data),
          Transport:send(Socket, <<(byte_size(Reply)):32/big, Reply/binary>>);
        {error, Reason} ->
          lager:error("read packet: ~p", [Reason]),
          Transport:close(Socket)
      end,
      loop(Socket, Transport);
    {error, Reason} ->
      lager:error("read length: ~p", [Reason]),
      ok = Transport:close(Socket)
  end.

decode_packet(Data) ->
  Request = bert:decode(Data),
  lager:debug("rpc in> ~p", [Request]),
  Reply = handle_packet(Request),
  lager:debug("rpc <out ~p", [Reply]),
  bert:encode(Reply).

handle_packet({call,ch_user_api,F,A})     -> erlang:apply(ch_user_api,F,A);
handle_packet({call,ch_clan_api,F,A})     -> erlang:apply(ch_clan_api,F,A);
handle_packet({call,ch_site_api,F,A})     -> erlang:apply(ch_site_api,F,A);
handle_packet({call,ch_acl_api,F,A})      -> erlang:apply(ch_acl_api,F,A);
handle_packet({call,ch_newsfeed_api,F,A}) -> erlang:apply(ch_newsfeed_api,F,A);
handle_packet({call,ch_forum_api,F,A})    -> erlang:apply(ch_forum_api,F,A);
handle_packet({call,ch_thread_api,F,A})   -> erlang:apply(ch_thread_api,F,A);
handle_packet({call,ch_post_api,F,A})     -> erlang:apply(ch_post_api,F,A).
