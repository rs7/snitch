-module(worker_pool_workers_supervisor).

-behaviour(supervisor).

%%% api
-export([start_link/0, start_child/1, terminate_child/1]).

%%% behaviour
-export([init/1]).

-include("../worker/worker.hrl").

%%%===================================================================
%%% api
%%%===================================================================

start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(WorkerId) -> supervisor:start_child(?MODULE, [WorkerId]).

terminate_child(WorkerId) ->
  Pid = gproc:get_value(?WORKER_PART_KEY(worker_supervisor, WorkerId), whereis(?MODULE)), %TODO я не уверен, что это сработает
  supervisor:terminate_child(?MODULE, Pid).

%%%===================================================================
%%% behaviour
%%%===================================================================

init([]) ->
  Strategy = #{strategy => simple_one_for_one, intensity => 0, period => 1},

  Specifications = [
    #{
      id => worker_item,
      start => {worker, start_link, []},
      type => worker
    }
  ],

  {ok, {Strategy, Specifications}}.
