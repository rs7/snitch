-module(connection_lib).

%%% api
-export([open/0, close/1, request/2]).

-define(DISABLE_KEEPALIVE, 16#7FFFFFF).

%%%===================================================================
%%% api
%%%===================================================================

open() ->
  gun:open("api.vk.com", 80, #{
    retry => 3,
    retry_timeout => 0,
    http_opts => #{keepalive => ?DISABLE_KEEPALIVE}
  }).

close(GunConnectionPid) -> gun:shutdown(GunConnectionPid).

request(GunConnectionPid, Request) -> request(GunConnectionPid, Request, post, []).

%%%===================================================================
%%% internal
%%%===================================================================

request(GunConnectionPid, {Method, Params}, get, Headers) ->
  gun:get(
    GunConnectionPid,
    request_lib:path_with_query(Method, Params),
    Headers
  );

request(GunConnectionPid, {Method, Params}, post, Headers) ->
  %lager:debug("request ~p ~p", [Method, Params]),

  folsom_metrics:new_counter(Method),
  folsom_metrics:tag_metric(Method, type),
  folsom_metrics:notify({Method, {inc, 1}}),

  folsom_metrics:notify(request, {inc, 1}),

  gun:post(
    GunConnectionPid,
    request_lib:path(Method),
    [{<<"Content-Type">>, <<"application/x-www-form-urlencoded">>} | Headers],
    request_lib:query(Params)
  ).
