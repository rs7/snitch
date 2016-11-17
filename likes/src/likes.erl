-module(likes).

-behaviour(application).
-behaviour(supervisor).

%%% behaviour application
-export([start/2, stop/1]).

%%% behaviour supervisor
-export([init/1]).

%%%===================================================================
%%% behaviour application
%%%===================================================================

start(_StartType, _StartArgs) -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

stop(_State) -> ok.

%%%===================================================================
%%% behaviour supervisor
%%%===================================================================

init([]) ->
  Strategy = #{strategy => one_for_all, intensity => 1, period => 5},

  {ok, RequesterCount} = application:get_env(requester_count),

  Specifications = [
    #{
      id => queue,
      start => {requeue, start_link, []},
      type => worker
    },
    #{
      id => requester_pool,
      start => {requester_pool, start_link, [RequesterCount]},
      type => worker
    }
  ],

  {ok, {Strategy, Specifications}}.
