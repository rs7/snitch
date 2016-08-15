-module(worker_supervisor).

-behaviour(supervisor).

%% api
-export([start_link/1]).

%% supervisor
-export([init/1]).

%%====================================================================
%% api
%%====================================================================

start_link(WorkerId) -> supervisor:start_link(?MODULE, WorkerId).

%%====================================================================
%% supervisor
%%====================================================================

init(WorkerId) ->
  Strategy = #{strategy => one_for_one, intensity => 5, period => 1},

  VkConnectionSpecification = #{
    id => vk_connection,
    start => {vk_connection, start_link, [WorkerId]},
    type => worker
  },

  ShotgunConnectionSpecification = #{
    id => shotgun_connection,
    start => {vk_connection, start_shotgun_connection, [WorkerId]},
    type => worker
  },

  {ok, {Strategy, [
    VkConnectionSpecification,
    ShotgunConnectionSpecification
  ]}}.
