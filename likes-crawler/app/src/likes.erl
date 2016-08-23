-module(likes).

-behaviour(application).

%%% api
-export([start/0, start/1, stop/0]).

%%% behaviour
-export([start/2, stop/1]).

%%%===================================================================
%%% api
%%%===================================================================

start() -> application:ensure_all_started(?MODULE).

start(debug) -> start(), set_log_level(debug).

stop() -> application:stop(?MODULE).

%%%===================================================================
%%% behaviour
%%%===================================================================

start(_StartType, []) -> likes_supervisor:start_link().

stop(_State) -> ok.

%%%===================================================================
%%% internal
%%%===================================================================

set_log_level(Level) -> lager:set_loglevel(lager_console_backend, Level).
