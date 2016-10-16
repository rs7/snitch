-module(conveyor).

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

  ListRef = make_ref(),

  Specifications = [
    #{
      id => list,
      start => {job_list, start_link, [ListRef]},
      type => supervisor
    },
    #{
      id => controller,
      start => {conveyor_controller, start_link, [ListRef]},
      type => worker
    }
  ],

  {ok, {Strategy, Specifications}}.
