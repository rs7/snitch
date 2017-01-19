-module(queue_controllers).

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
  Strategy = #{strategy => one_for_all},

  Specifications = [
    #{
      id => user_1M_controller,
      start => {gen_queue_controller, start_link, [user_1M_controller]}
    },
    #{
      id => user_10K_controller,
      start => {gen_queue_controller, start_link, [user_10K_controller]}
    }
  ],

  {ok, {Strategy, Specifications}}.
