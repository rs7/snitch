-module(debug).

%%% api
-export([start/0]).

%%%===================================================================
%%% api
%%%===================================================================

start() ->
  application:ensure_all_started(gun),
  application:ensure_all_started(lager),
  %lager:set_loglevel(lager_console_backend, debug),

  connection_server:start_link().

