-module(catalog_queue_controllers).

-behaviour(supervisor).

%%% api
-export([start_link/0]).

%%% behaviour
-export([init/1]).

-define(RUN, <<"snitch.run">>).
-define(USER_1M, <<"snitch.user_1M">>).
-define(USER_10K, <<"snitch.user_10K">>).
-define(USER_100, <<"snitch.user_100">>).
-define(USER, <<"snitch.user">>).

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
      id => user_1M_controller,
      start => {catalog_queue_controller, start_link, [#{
        queue => ?USER_1M,
        source => ?RUN,
        catalog_type => e6,
        watch_timeout => 60000,
        length_min => 1,
        length_normal => 400
      }]}
    },
    #{
      id => user_10K_controller,
      start => {catalog_queue_controller, start_link, [#{
        queue => ?USER_10K,
        source => ?USER_1M,
        catalog_type => e4,
        watch_timeout => 10000,
        length_min => 25,
        length_normal => 75
      }]}
    },
    #{
      id => user_100_controller,
      start => {catalog_queue_controller, start_link, [#{
        queue => ?USER_100,
        source => ?USER_10K,
        catalog_type => e2,
        watch_timeout => 5000,
        length_min => 50,
        length_normal => 150
      }]}
    },
    #{
      id => user_controller,
      start => {catalog_queue_controller, start_link, [#{
        queue => ?USER,
        source => ?USER_100,
        catalog_type => e0,
        watch_timeout => 300,
        length_min => 10000,
        length_normal => 10300
      }]}
    }
  ],

  {ok, {Strategy, Specifications}}.
