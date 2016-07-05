-module(vk_call).

-include("vk_request.hrl").

-define(RESPONSE_KEY, <<"response">>).

%% API exports
-export([call/1, callAll/1]).

%%====================================================================
%% API functions
%%====================================================================

call(Request) -> response(decode(body(request(url(Request))))).

callAll(Requests) -> rpc:pmap({?MODULE, call}, [], Requests).

%%====================================================================
%% Internal functions
%%====================================================================

url(Request) -> "http://api.vk.com/method/" ++ atom_to_list(Request#request.method) ++ "?" ++ query(Request#request.params).

query(Params) -> string:join(
  [lists:concat([Key, "=", Value]) || {Key, Value} <- maps:to_list(Params)],
  "&"
).

request(URL) ->
  io:format("--> ~s~n", [URL]), %%debug
  httpc:request(URL).

body({ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}}) -> Body.

decode(Body) -> jsone:decode(list_to_binary(Body)).

response(#{?RESPONSE_KEY := Response}) -> Response.
