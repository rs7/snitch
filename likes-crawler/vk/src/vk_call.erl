-module(vk_call).

-include("vk_request.hrl").

-define(RESPONSE_KEY, <<"response">>).

%% API exports
-export([call/2]).

%%====================================================================
%% API functions
%%====================================================================

call(Request, Method) -> result(request(Request, Method)).

%%====================================================================
%% Internal functions
%%====================================================================

url(Method) -> "http://api.vk.com/method/" ++ atom_to_list(Method).

query(Params) -> P = string:join(
  [pair(Key, Value) || {Key, Value} <- maps:to_list(Params)],
  "&"
), io:format("~s~n", [P]), P.

pair(Key, Value) -> lists:concat([Key, "=", Value]).

request(#request{method = Method, params = Params}, HTTPMethod) -> request(
  url(Method),
  query(Params),
  HTTPMethod
).

request(URL, Query, get) -> httpc:request(URL ++ "?" ++ Query);
request(URL, Query, post) -> httpc:request(
  post,
  {
    URL, [], "application/x-www-form-urlencoded", Query
  },
  [], []
).

result(Result) -> response(decode(body(Result))).

body({ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}}) -> Body.

decode(Body) -> jsone:decode(list_to_binary(Body)).

response(#{?RESPONSE_KEY := Response}) -> Response.
