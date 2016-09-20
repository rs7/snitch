-module(worker).

%%% api
-export([start_link/1]).

%%%===================================================================
%%% api
%%%===================================================================

start_link(WorkerId) -> worker_supervisor:start_link(WorkerId).
