-module(debug).

%%% api
-export([start/1]).

%%%===================================================================
%%% api
%%%===================================================================

start(WorkerCount) ->
  application:ensure_all_started(gun),
  application:ensure_all_started(lager),

  request_counter:start_link(),

  lager:set_loglevel(lager_console_backend, info),

  [
    worker_supervisor:start_link() ||
    _ <- lists:seq(1, WorkerCount)
  ].

