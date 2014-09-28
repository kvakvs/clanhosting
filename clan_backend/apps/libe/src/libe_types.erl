%%%-------------------------------------------------------------------
%%% @doc Type conversions
%%% @end
%%% Created : 25. Aug 2014 22:56
%%%-------------------------------------------------------------------
-module(libe_types).

%% API
-export([as_binary/1, as_string/1, as_int/1]).

as_string(X) when is_list(X)    -> X;
as_string(X) when is_binary(X)  -> binary_to_list(X);
as_string(X) when is_integer(X) -> integer_to_list(X);
as_string(X) when is_atom(X)    -> atom_to_list(X).

as_binary({bert, nil})          -> <<>>;
as_binary(X) when is_binary(X)  -> X;
as_binary(X) when is_list(X)    -> list_to_binary(X);
as_binary(X) when is_integer(X) -> as_binary(integer_to_list(X));
as_binary(X) when is_atom(X)    -> atom_to_binary(X, latin1).

as_int(X) when is_integer(X) -> X;
as_int(X) when is_binary(X)  -> binary_to_integer(X);
as_int(X) when is_list(X)    -> list_to_integer(X);
as_int(X) when is_atom(X)    -> atom_to_list(list_to_integer(X)).
