-module(vk_supervisor).

-behaviour(supervisor).

%% api
-export([start_link/1]).

%% supervisor
-export([init/1]).

%%====================================================================
%% api
%%====================================================================

start_link(RequestersCount) -> supervisor:start_link({local, ?MODULE}, ?MODULE, RequestersCount).

%%====================================================================
%% supervisor
%%====================================================================

init(RequestersCount) ->
  Strategy = #{strategy => one_for_all, intensity => 0, period => 1},

  RequestPoolSpecification = #{
    id => vk_request_generator,
    start => {vk_request_generator, start_link, []},
    type => supervisor
  },

  RequesterPoolSupervisorSpecification = #{
    id => vk_requester_pool_supervisor,
    start => {vk_requester_pool_supervisor, start_link, []},
    type => supervisor
  },

  RequesterPoolControllerSpecification = #{
    id => vk_requester_pool_controller,
    start => {vk_requester_pool_controller, start_link, [RequestersCount]}
  },

  {ok, {Strategy, [
    RequestPoolSpecification,
    RequesterPoolSupervisorSpecification,
    RequesterPoolControllerSpecification
  ]}}.
