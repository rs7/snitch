-module(request).

-export([create/1]).

create(Requests) -> request(Requests, <<>>).

request([Request], Send) ->
  Packet = packet(Request, close),
  <<Send/binary, Packet/binary>>;

request([Request | Remaining], Send) ->
  Packet = packet(Request, continue),
  request(Remaining, <<Send/binary, Packet/binary>>).

packet({Method, Params}, ProceedFlag) ->
  CloseHeader = case ProceedFlag of
    close -> <<"Connection: close\n">>;
    continue -> <<>>
  end,
  Path = atom(Method),
  Query = query(Params),
  <<
    "GET /method/", Path/binary, $?, Query/binary, " HTTP/1.1\n",
    "Host: api.vk.com\n",
    CloseHeader/binary,
    "\n"
  >>.

query(Params) ->
  list(
    [<<(atom(Key))/binary, $=, (value(Value))/binary>> || {Key, Value} <- maps:to_list(Params)],
    $&
  ).

value(Value) when is_atom(Value) -> atom(Value);
value(Value) when is_integer(Value) -> integer_to_binary(Value);
value(Value) when is_list(Value) -> list(lists:map(fun value/1, Value), $,).

atom(Atom) -> atom_to_binary(Atom, latin1).

list(List, Separator) -> list_to_binary(lists:join(Separator, List)).
