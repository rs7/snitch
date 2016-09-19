-module(worker_pool_workers_supervisor).

-behaviour(supervisor).

%%% api
-export([start_link/0, get_workers_pids/0, start_child/0, terminate_child/1, get_workers_count/0]).

%%% behaviour
-export([init/1]).

%%%===================================================================
%%% api
%%%===================================================================

start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

get_workers_pids() ->
  [Pid || {_, Pid, _, _} <- supervisor:which_children(?MODULE)].

start_child() -> supervisor:start_child(?MODULE, []).

terminate_child(Pid) -> supervisor:terminate_child(?MODULE, Pid).

get_workers_count() -> maps:get(workers, maps:from_list(supervisor:count_children(?MODULE))).

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
