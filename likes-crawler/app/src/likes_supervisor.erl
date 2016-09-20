-module(likes_supervisor).

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
  Strategy = #{strategy => one_for_all, intensity => 0, period => 1},

  Specifications = [
    #{
      id => root_heap,
      start => {root_heap, start_link, []},
      type => worker
    },
    #{
      id => local_heap,
      start => {heap, start_link, [{local, local_heap}, {global, root_heap}, {5000, 50000, 100000}]},
      type => worker
    },
    #{
      id => worker_pool,
      start => {worker_pool, start_link, []},
      type => supervisor
    },
    #{
      id => stat,
      start => {stat, start_link, []},
      type => worker
    }
  ],

  {ok, {Strategy, Specifications}}.
