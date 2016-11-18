-module(test).

-behaviour(supervisor).

%%% api
-export([start_link/0]).

%%% behaviour
-export([init/1]).

%%%===================================================================
%%% api
%%%===================================================================

start_link() -> supervisor:start_link(?MODULE, []).

%%%===================================================================
%%% behaviour
%%%===================================================================

init([]) ->
  Strategy = #{strategy => one_for_all},

  Specifications = [
    #{
      id => pool,
      start => {test_pool, start_link, [20]}
    },
    #{
      id => metrics,
      start => {metrics, start_link, []}
    }
  ],

  {ok, {Strategy, Specifications}}.
