%%%-------------------------------------------------------------------
%%% @doc Type conversions
%%% @end
%%% Created : 25. Aug 2014 22:56
%%%-------------------------------------------------------------------
-module(libe_types).

%% API
-export([as_binary/1]).

as_binary(X) when is_binary(X) -> X;
as_binary(X) when is_list(X) -> list_to_binary(X);
as_binary(X) when is_integer(X) -> as_binary(integer_to_list(X));
as_binary(X) when is_atom(X) -> atom_to_binary(X, latin1).
