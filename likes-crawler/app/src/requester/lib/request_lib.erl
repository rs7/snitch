-module(request_lib).

%%% api
-export([path/1, query/1, path_with_query/2]).

%%%===================================================================
%%% api
%%%===================================================================

path(Method) -> [<<"/method/">>, atom_to_binary(Method)].

query(Params) -> list_to_binary(
  lists:map(fun key_value_to_binary/1, maps:to_list(Params)),
  $&
).

path_with_query(Method, Params) when map_size(Params) == 0 -> path(Method);
path_with_query(Method, Params) -> [path(Method), $?, query(Params)].

%%%===================================================================
%%% internal
%%%===================================================================

key_value_to_binary({Key, Value}) -> [atom_to_binary(Key), $=, value_to_binary(Value)].

value_to_binary(Value) when is_atom(Value) -> atom_to_binary(Value);
value_to_binary(Value) when is_integer(Value) -> integer_to_binary(Value);
value_to_binary(Value) when is_list(Value) -> list_to_binary(lists:map(fun value_to_binary/1, Value), $,).

atom_to_binary(Atom) when is_atom(Atom) -> atom_to_binary(Atom, latin1).

list_to_binary(List, Separator) when is_list(List) -> lists:join(Separator, List).
