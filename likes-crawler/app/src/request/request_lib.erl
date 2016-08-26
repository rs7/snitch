-module(request_lib).

%%% api
-export([]).

%%%===================================================================
%%% api
%%%===================================================================

%%%===================================================================
%%% internal
%%%===================================================================

run(Connection, {Method, Params}) ->
  gun:post(
    Connection,
    ["/method/", atom_to_list(Method)],
    [{<<"Content-Type">>, <<"application/x-www-form-urlencoded">>}],
    to_urlencoded(Params)
  ).

parse_body(Body) ->
  case jsone:try_decode(Body) of
    {ok, #{<<"response">> := Response}, <<>>} -> {ok, {response, Response}};
    {ok, #{<<"error">> := #{<<"error_code">> := ErrorCode}}, <<>>} -> {ok, {error, ErrorCode}};
    {ok, Result, <<>>} -> {error, {unexpected_result, Result}};
    {ok, _Result, Remainings} -> {error, {unexpected_remainings, Remainings}};
    {error, Reason} -> {error, Reason}
  end.

to_urlencoded(Params) -> string:join(
  [atom_to_list(Key) ++ "=" ++ to_list(Value) || {Key, Value} <- maps:to_list(Params)], "&"
).

to_list(Atom) when is_atom(Atom) -> atom_to_list(Atom);
to_list(Integer) when is_integer(Integer) -> integer_to_list(Integer);
to_list(List) when is_list(List) -> string:join(lists:map(fun to_list/1, List), ",").

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


