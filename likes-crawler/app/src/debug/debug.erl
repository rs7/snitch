-module(debug).

%%% api
-export([start/1]).

%%%===================================================================
%%% api
%%%===================================================================

start(WorkerCount) ->
  application:ensure_all_started(gun),
  application:ensure_all_started(lager),
  application:ensure_all_started(gproc),
  %lager:set_loglevel(lager_console_backend, debug),

  [
    worker_supervisor:start_link(Id) ||
    Id <- lists:seq(1, WorkerCount)
  ].

