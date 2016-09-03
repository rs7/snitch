-module(debug).

%%% api
-export([start/3]).

%%%===================================================================
%%% api
%%%===================================================================

start(WorkerCount, Delay, LogLevel) ->
  application:ensure_all_started(gun),
  application:ensure_all_started(lager),

  request_counter:start_link(),

  lager:set_loglevel(lager_console_backend, LogLevel),

  start_workers(WorkerCount, Delay).

%%%===================================================================
%%% internal
%%%===================================================================

start_workers(0, _Delay) -> ok;

start_workers(N, Delay) ->
  {ok, Pid} = worker_supervisor:start_link(),
  lager:debug("worker started ~p", [Pid]),
  timer:sleep(Delay),
  start_workers(N - 1, Delay).
