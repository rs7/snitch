-module(worker_pool_supervisor).

-behaviour(supervisor).

%%% api
-export([start_link/0]).

%%% behaviour
-export([init/1]).

%%%===================================================================
%%% api
%%%===================================================================

start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%===================================================================
%%% behaviour
%%%===================================================================

init([]) ->
  Strategy = #{strategy => one_for_all, intensity => 1, period => 5},

  Specifications = [
    #{
      id => workers_supervisor,
      start => {worker_pool_workers_supervisor, start_link, []},
      type => supervisor
    },
    #{
      id => controller,
      start => {worker_pool_controller, start_link, []},
      type => worker
    }
  ],

  {ok, {Strategy, Specifications}}.
