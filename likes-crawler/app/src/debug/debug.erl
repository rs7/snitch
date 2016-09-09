-module(debug).

%%% api
-export([start/0]).

%%%===================================================================
%%% general
%%%===================================================================

start() ->
  likes:start(),
  lager:set_loglevel(lager_console_backend, debug).
