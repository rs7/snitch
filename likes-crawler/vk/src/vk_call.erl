-module(vk_call).

-include("vk_request.hrl").

-define(ERROR_KEY, <<"error">>).
-define(ERROR_CODE_KEY, <<"error_code">>).
-define(ERROR_MESSAGE_KEY, <<"error_msg">>).
-define(RESPONSE_KEY, <<"response">>).

%% API exports
-export([call/2]).

%%====================================================================
%% API functions
%%====================================================================

call(Request, Method) -> {response, Response} = result(request(Request, Method)), Response.

%%====================================================================
%% Internal functions
%%====================================================================

url(Method) -> "http://api.vk.com/method/" ++ atom_to_list(Method).

query(Params) ->
  Query = string:join(
    [lists:concat([Key, "=", Value]) || {Key, Value} <- maps:to_list(Params)], "&"
  ),
  io:format("~s~n", [Query]), %%debug
  Query.

request(#request{method = Method, params = Params}, HTTPMethod) -> request(
  url(Method),
  query(Params),
  HTTPMethod
).

request(URL, Query, get) -> httpc:request(get, {URL ++ "?" ++ Query, []}, [], []);
request(URL, Query, post) -> httpc:request(post, {URL, [], "application/x-www-form-urlencoded", Query}, [], []).

result({error, Reason}) -> {error, {http, Reason}};
result({ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}}) ->
  case jsone:try_decode(list_to_binary(Body)) of
    {error, Error} -> {error, {json, Error}};
    {ok, #{?ERROR_KEY := Error}, _} -> {error, {vk, errorObject(Error)}};
    {ok, #{?RESPONSE_KEY := Response}, _} -> {response, Response}
  end;
result({ok, {{_Version, StatusCode, ReasonPhrase}, _Headers, _Body}}) -> {error, {httpStatus, {StatusCode, ReasonPhrase}}}.

errorObject(#{?ERROR_CODE_KEY := Code, ?ERROR_MESSAGE_KEY := Message}) -> {Code, binary_to_list(Message)}.
