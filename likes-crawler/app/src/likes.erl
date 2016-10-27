-module(likes).

-behaviour(application).

%%% behaviour
-export([start/2, stop/1]).

%%%===================================================================
%%% behaviour
%%%===================================================================

start(_StartType, []) ->
  task_queue:start(),
  likes_supervisor:start_link().

stop(_State) -> ok.
