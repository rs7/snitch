-module(pool).

-behaviour(supervisor).

%%% api
-export([start_link/1]).

%%% supervisor
-export([init/1]).

%%%===================================================================
%%% api
%%%===================================================================

start_link(Process) -> supervisor:start_link({local, ?MODULE}, ?MODULE, [Process]).

%%%===================================================================
%%% supervisor
%%%===================================================================

init([Process]) ->
  Strategy = #{strategy => one_for_all, intensity => 1, period => 5},

  Specifications = [
    #{
      id => supervisor,
      start => {pool_supervisor, start_link, [Process]},
      type => supervisor
    },
    #{
      id => controller,
      start => {pool_controller, start_link, []},
      type => worker
    }
  ],

  {ok, {Strategy, Specifications}}.
