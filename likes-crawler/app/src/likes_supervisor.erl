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
  Strategy = #{strategy => one_for_all, intensity => 5, period => 1},

  {ok, RequesterCount} = application:get_env(requester_count),
  {ok, StatTimeout} = application:get_env(stat_timeout),

  {ok, ConveyorJobsInOneTime} = application:get_env(jobs_in_one_time_count),
  {ok, MetricsPort} = application:get_env(metrics_port),

  ElliOpts = [{callback, metrics_http}, {port, MetricsPort}],

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
      id => conveyor,
      start => {conveyor, start_link, [ConveyorJobsInOneTime]},
      type => supervisor
    },
    #{
      id => elli,
      start => {elli, start_link, [ElliOpts]},
      type => worker
    },
    #{
      id => stat,
      start => {stat, start_link, [StatTimeout]},
      type => worker
    }
  ],

  {ok, {Strategy, Specifications}}.
