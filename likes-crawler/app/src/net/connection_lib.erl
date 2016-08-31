-module(connection_lib).

%%% api
-export([open/0, close/1, request/2]).

-define(DISABLE_KEEPALIVE, 16#7FFFFFF).

%%%===================================================================
%%% api
%%%===================================================================

open() -> gun:open("api.vk.com", 80, #{
  retry => 3,
  retry_timeout => 0,
  http_opts => #{keepalive => ?DISABLE_KEEPALIVE}
}).

close(GunConnectionPid) -> gun:shutdown(GunConnectionPid).

request(GunConnectionPid, Request) ->
  request(GunConnectionPid, Request, get).

%%%===================================================================
%%% internal
%%%===================================================================

request(GunConnectionPid, {Method, Params}, get) ->
  gun:get(
    GunConnectionPid,
    request_lib:path_with_query(Method, Params)
  );

request(GunConnectionPid, {Method, Params}, post) ->
  gun:post(
    GunConnectionPid,
    request_lib:path(Method),
    [{<<"Content-Type">>, <<"application/x-www-form-urlencoded">>}],
    request_lib:query(Params)
  ).
