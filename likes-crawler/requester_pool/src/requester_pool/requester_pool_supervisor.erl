-module(requester_pool_supervisor).

-behaviour(supervisor).

%%% api
-export([start_link/1]).

%%% behaviour
-export([init/1]).

%%%===================================================================
%%% api
%%%===================================================================

start_link(Size) -> supervisor:start_link({local, ?MODULE}, ?MODULE, Size).

%%%===================================================================
%%% behaviour
%%%===================================================================

init(Size) ->
  Strategy = #{strategy => one_for_all, intensity => 1, period => 5},

  Specifications = [
    #{
      id => children,
      start => {requester_pool_children, start_link, []},
      type => supervisor
    },
    #{
      id => controller,
      start => {requester_pool_controller, start_link, [Size]},
      type => worker
    }
  ],

  {ok, {Strategy, Specifications}}.
