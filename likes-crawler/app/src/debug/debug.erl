-module(debug).

%%% api
-export([init/0, call/1, start/3]).

%%%===================================================================
%%% general
%%%===================================================================

init() -> init(debug).

init(LogLevel) ->
  {ok, _} = application:ensure_all_started(lager),
  {ok, _} = application:ensure_all_started(gun),
  ok = lager:set_loglevel(lager_console_backend, LogLevel).

call([RequestData]) -> [call(RequestData)];

call([_ | _] = RequestDataList) ->
  rpc:parallel_eval([
    {?MODULE, call, [RequestData]} || RequestData <- RequestDataList
  ]);

call(RequestData) ->
  lager:debug("debug:call ~p", [RequestData]),
  {ok, ConnectionPid} = connection_lib:open(),
  {ok, Body} = gun:await_body(ConnectionPid, connection_lib:request(ConnectionPid, RequestData)),
  connection_lib:close(ConnectionPid),
  {ok, Result} = response_lib:decode_body(Body),
  Result.

%%%===================================================================
%%% application
%%%===================================================================

start(WorkerCount, Delay, LogLevel) ->
  init(LogLevel),
  start_workers(WorkerCount, Delay).

start_workers(0, _Delay) -> ok;

start_workers(N, Delay) ->
  {ok, Pid} = worker_supervisor:start_link(),
  lager:info("worker started ~p", [Pid]),
  timer:sleep(Delay),
  start_workers(N - 1, Delay).
