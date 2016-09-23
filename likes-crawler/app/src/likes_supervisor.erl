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

  TestJobData = {request_job, {get_friends, test:which_search()}},

  {ok, RequesterCount} = application:get_env(requester_count),
  {ok, StatTimeout} = application:get_env(stat_timeout),

  Specifications = [
    #{
      id => call_queue,
      start => {call_queue, start_link, []},
      type => worker
    },
    #{
      id => requester_pool,
      start => {requester_pool, start_link, [RequesterCount]},
      type => worker
    },
    #{
      id => controller,
      start => {test, start_link, [local]},
      type => worker
    },
    #{
      id => job,
      start => {job, start_link, [test, [1], make_ref(), TestJobData]},
      type => supervisor
    },
    #{
      id => stat,
      start => {stat, start_link, [StatTimeout]},
      type => worker
    }
  ],

  {ok, {Strategy, Specifications}}.
