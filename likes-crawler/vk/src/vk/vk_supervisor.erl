-module(vk_supervisor).

-behaviour(supervisor).

%% api
-export([start_link/0]).

%% supervisor
-export([init/1]).

%%====================================================================
%% api
%%====================================================================

start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%====================================================================
%% supervisor
%%====================================================================

init([]) ->
  Strategy = #{strategy => one_for_all, intensity => 0, period => 1},

  RequestPoolSpecification = #{
    id => vk_request_pool,
    start => {vk_request_pool, start_link, []},
    type => supervisor
  },

  RequesterPoolSupervisorSpecification = #{
    id => vk_requester_pool_supervisor,
    start => {vk_requester_pool_supervisor, start_link, []},
    type => supervisor
  },

  RequesterPoolControllerSpecification = #{
    id => vk_requester_pool_controller,
    start => {vk_requester_pool_controller, start_link, []}
  },

  {ok, {Strategy, [
    RequestPoolSpecification,
    RequesterPoolSupervisorSpecification,
    RequesterPoolControllerSpecification
  ]}}.
