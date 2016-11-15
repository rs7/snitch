-module(vk).

%%%api
-export([call/1]).

%%%===================================================================
%%% api
%%%===================================================================

call(Request) ->
  {ok, [Response]} = socket:start(request(Request)),
  response(Response).
  %Response.

%%%===================================================================
%%% internal
%%%===================================================================

response(
  <<
    "HTTP/1.1 200 OK",
    _:(300 * 8),
    Body/binary
  >>
) -> body(Body);

response(_Response) -> fail(http_error).

body(Body) -> decode_result(jsone:try_decode(Body)).

decode_result({ok, Result, <<>>}) -> result(Result);
decode_result({ok, _Result, _Remainings}) -> fail(json_error);
decode_result({error, _Reason}) -> fail(json_error).

result(#{<<"response">> := Response}) -> success({response, Response});
result(#{<<"error">> := #{<<"error_code">> := ErrorCode}}) -> vk_error(ErrorCode);
result(_) -> fail(unexpected_result).

vk_error(1) -> fail(vk_error);
vk_error(10) -> fail(vk_error);
vk_error(ErrorCode) -> success({error, ErrorCode}).

success(Result) -> {ok, Result}.

fail(Reason) -> {error, Reason}.

request({Method, Params}) ->
  <<
    "GET /method/", (atom(Method))/binary, $?, (query(Params))/binary, " HTTP/1.1",
    "\nHost: api.vk.com",
    "\nConnection: close",
    "\n\n"
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
