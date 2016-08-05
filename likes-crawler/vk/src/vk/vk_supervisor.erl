-module(vk_supervisor).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% supervisor
-export([init/1]).

%%====================================================================
%% API
%%====================================================================

start_link(ConnectionCount) -> supervisor:start_link({local, ?MODULE}, ?MODULE, ConnectionCount).

%%====================================================================
%% supervisor
%%====================================================================

init(ConnectionCount) ->
  Strategy = #{strategy => one_for_all, intensity => 0, period => 1},

  PoolSupervisorSpecification = #{
    id => vk_connection_pool_supervisor,
    start => {vk_connection_pool_supervisor, start_link, []},
    type => supervisor
  },

  PoolControllerSpecification = #{
    id => vk_connection_pool_controller,
    start => {vk_connection_pool_controller, start_link, [ConnectionCount]}
  },

  {ok, {Strategy, [
    PoolSupervisorSpecification,
    PoolControllerSpecification
  ]}}.
