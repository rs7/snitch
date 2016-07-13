-module(vk_call).

-define(ERROR_KEY, <<"error">>).
-define(ERROR_CODE_KEY, <<"error_code">>).
-define(ERROR_MESSAGE_KEY, <<"error_msg">>).
-define(RESPONSE_KEY, <<"response">>).

%% API exports
-export([call/1, callAll/1]).

%%====================================================================
%% API functions
%%====================================================================

call(Request) -> {response, Response} = result(request(Request)), Response.

callAll(Requests) -> rpc:pmap({?MODULE, call}, [], Requests).

%%====================================================================
%% Internal functions
%%====================================================================

request({Method, Params}) -> httpc:request(post, {url(Method), [], "application/x-www-form-urlencoded", query(Params)}, [], []).

url(Method) -> "http://" ++ vk_api:domain() ++ "/" ++ vk_api:method(Method).

query(Params) -> string:join(
  [atom_to_list(Key) ++ "=" ++ vk_param:to_value(Value) || {Key, Value} <- maps:to_list(Params)], "&"
).

result({error, Reason}) -> {error, {http, Reason}};
result({ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}}) ->
  case jsone:try_decode(list_to_binary(Body)) of
    {error, Error} -> {error, {json, Error}};
    {ok, #{?ERROR_KEY := Error}, _} -> {error, {vk, errorObject(Error)}};
    {ok, #{?RESPONSE_KEY := Response}, _} -> {response, Response}
  end;
result({ok, {{_Version, StatusCode, ReasonPhrase}, _Headers, _Body}}) -> {error, {httpStatus, {StatusCode, ReasonPhrase}}}.

errorObject(#{?ERROR_CODE_KEY := Code, ?ERROR_MESSAGE_KEY := Message}) -> {Code, binary_to_list(Message)}.
