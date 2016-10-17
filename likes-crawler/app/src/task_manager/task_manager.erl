-module(task_manager).

%%% api
-export([]).

%%%===================================================================
%%% api
%%%===================================================================

execute(Task) -> ok.

reserve(Type, Count) -> ok.

release(ReserveRef, Result) -> ok.

