-module(likes).

-behaviour(application).

%%% api
-export([start/0, stop/0]).

%%% behaviour
-export([start/2, stop/1]).

%%%===================================================================
%%% api
%%%===================================================================

start() -> application:ensure_all_started(?MODULE).

stop() -> application:stop(?MODULE).

%%%===================================================================
%%% behaviour
%%%===================================================================

start(_StartType, []) ->
  task_manager:start(),
  likes_supervisor:start_link().

stop(_State) -> ok.
