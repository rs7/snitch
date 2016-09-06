-module(debug).

%%% api
-export([start/3, call/1, init/0]).

%%%===================================================================
%%% api
%%%===================================================================

start(WorkerCount, Delay, LogLevel) ->
  application:ensure_all_started(gun),
  application:ensure_all_started(lager),

  lager:set_loglevel(lager_console_backend, LogLevel),

  start_workers(WorkerCount, Delay).

call([RequestData]) -> [call(RequestData)];

call([_ | _] = RequestDataList) -> rpc:parallel_eval([
  {?MODULE, call, RequestData} || RequestData <- RequestDataList
]);

call(RequestData) ->
  lager:debug("call ~p", [RequestData]),
  {ok, ConnectionPid} = connection_lib:open(),
  {ok, Body} = gun:await_body(ConnectionPid, connection_lib:request(ConnectionPid, RequestData)),
  connection_lib:close(ConnectionPid),
  {ok, Result} = response_lib:decode_body(Body),
  Result.

init() ->
  application:ensure_all_started(gun),
  application:ensure_all_started(lager),
  lager:set_loglevel(lager_console_backend, debug).

%%%===================================================================
%%% internal
%%%===================================================================

start_workers(0, _Delay) -> ok;

start_workers(N, Delay) ->
  {ok, Pid} = worker_supervisor:start_link(),
  lager:info("worker started ~p", [Pid]),
  timer:sleep(Delay),
  start_workers(N - 1, Delay).
