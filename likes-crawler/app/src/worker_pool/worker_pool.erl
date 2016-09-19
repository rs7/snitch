-module(worker_pool).

%%% api
-export([start_link/0, get_workers_count/0]).

%%%===================================================================
%%% api
%%%===================================================================

start_link() -> worker_pool_supervisor:start_link().

get_workers_count() -> worker_pool_workers_supervisor:get_workers_count().
