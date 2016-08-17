-module(pool).

-behaviour(supervisor).

%%% api
-export([start_link/0]).

%%% supervisor
-export([init/1]).

%%%===================================================================
%%% api
%%%===================================================================

start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%===================================================================
%%% supervisor
%%%===================================================================

init([]) ->
  Strategy = #{strategy => one_for_all, intensity => 1, period => 5},

  Specifications = [
    #{
      id => supervisor,
      start => {pool_supervisor, start_link, []},
      type => supervisor
    },
    #{
      id => controller,
      start => {pool_controller, start_link, []},
      type => worker
    }
  ],

  {ok, {Strategy, Specifications}}.
