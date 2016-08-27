-module(request_lib).

%%% api
-export([open/0, close/1, request/3]).

-define(DISABLE_KEEPALIVE, 16#7FFFFFF).

%%%===================================================================
%%% api
%%%===================================================================

open() -> gun:open("api.vk.com", 80, #{
  retry => 1,
  retry_timeout => 0,
  http_opts => #{keepalive => ?DISABLE_KEEPALIVE}
}).

close(Connection) -> gun:shutdown(Connection).

request(Connection, {Method, Params}, get) ->
  gun:get(
    Connection,
    path_with_params(Method, Params)
  );

request(Connection, {Method, Params}, post) ->
  gun:post(
    Connection,
    method_to_endpoint(Method),
    [{<<"Content-Type">>, <<"application/x-www-form-urlencoded">>}],
    to_urlencoded(Params)
  ).

%%%===================================================================
%%% internal
%%%===================================================================

path_with_params(Method, Params) when map_size(Params) == 0 -> method_to_endpoint(Method);
path_with_params(Method, Params) -> [method_to_endpoint(Method), $?, to_urlencoded(Params)].

method_to_endpoint(Method) -> [<<"/method/">>, atom_to_binary(Method)].

to_urlencoded(Params) -> list_to_binary(
  lists:map(fun key_value_to_binary/1, maps:to_list(Params)),
  $&
).

key_value_to_binary({Key, Value}) -> [atom_to_binary(Key), $=, value_to_binary(Value)].

value_to_binary(Value) when is_atom(Value) -> atom_to_binary(Value);
value_to_binary(Value) when is_integer(Value) -> integer_to_binary(Value);
value_to_binary(Value) when is_list(Value) -> list_to_binary(lists:map(fun value_to_binary/1, Value), $,).

atom_to_binary(Atom) when is_atom(Atom) -> atom_to_binary(Atom, latin1).

list_to_binary(List, Separator) when is_list(List) -> lists:join(Separator, List).

parse_body(Body) ->
  case jsone:try_decode(Body) of
    {ok, #{<<"response">> := Response}, <<>>} -> {ok, {response, Response}};
    {ok, #{<<"error">> := #{<<"error_code">> := ErrorCode}}, <<>>} -> {ok, {error, ErrorCode}};
    {ok, Result, <<>>} -> {error, {unexpected_result, Result}};
    {ok, _Result, Remainings} -> {error, {unexpected_remainings, Remainings}};
    {error, Reason} -> {error, Reason}
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%repeatable_request(Connection, Request, RetryNumber) ->
%%  lager:debug("~B/~p", [RetryNumber, Request]),
%%  case run(Connection, Request) of
%%    {result, Result} -> Result;
%%    {retry, Reason} ->
%%      error_warning(Reason, Request, RetryNumber),
%%      timer:sleep(retry_timeout(RetryNumber)),
%%      repeatable_request(Connection, Request, RetryNumber + 1)
%%  end.
%%
%%request(Connection, Request) ->
%%  Result = shotgun_request(Connection, Request),
%%  case Result of
%%    {ok, #{status_code := 200, body := Body}} -> parse_body(Body);
%%    {ok, #{status_code := StatusCode}} -> {retry, {status_code, StatusCode}};
%%    {error, Reason} -> {retry, {shotgun, Reason}}
%%  end.
%%
%%
%%
%%vk_error(1) -> {retry, {vk_error, 1}};
%%vk_error(10) -> {retry, {vk_error, 10}};
%%vk_error(Code) -> {result, {error, Code}}.
%%
%%retry_timeout(1) -> 10 * 1000;
%%retry_timeout(2) -> rand_seconds(30, 60);
%%retry_timeout(3) -> rand_seconds(60, 120);
%%retry_timeout(5) -> rand_seconds(120, 300).
%%
%%error_warning(_Reason, _Request, TryNumber) when TryNumber < 5 -> ok;
%%error_warning(Reason, Request, TryNumber) -> lager:warning(
%%  "Request error ~B times~n~p~n~p", [TryNumber, Request, Reason]
%%).

%%%===================================================================
%%% util
%%%===================================================================

rand_seconds(From, To) -> (From + rand:uniform(To - From)) * 1000.


