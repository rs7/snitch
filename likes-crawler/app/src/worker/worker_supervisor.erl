-module(worker_supervisor).

-behaviour(supervisor).

%%% api
-export([start_link/1]).

%%% behaviour
-export([init/1]).

-include("./worker.hrl").

%%%===================================================================
%%% api
%%%===================================================================

start_link(WorkerId) -> supervisor:start_link(?WORKER_PART_NAME(?MODULE, WorkerId), ?MODULE, WorkerId).

%%%===================================================================
%%% behaviour
%%%===================================================================

init(WorkerId) ->
  Strategy = #{strategy => one_for_all, intensity => 0, period => 1},

  Specifications = [
    #{
      id => heap,
      start => {heap, start_link, [?WORKER_PART_NAME(heap, WorkerId), local_heap, {100, 300, 600}]},
      type => worker
    },
    #{
      id => requester,
      start => {requester, start_link, [WorkerId]},
      type => worker
    },
    #{
      id => connection,
      start => {connection, start_link, [WorkerId]},
      type => worker
    }
  ],

  {ok, {Strategy, Specifications}}.
