-module(requester_pool).

-behaviour(supervisor).

%%% api
-export([start_link/1, get_size/0, set_size/1]).

%%% behaviour
-export([init/1]).

%%%===================================================================
%%% api
%%%===================================================================

start_link(Size) -> supervisor:start_link({local, ?MODULE}, ?MODULE, Size).

get_size() -> requester_pool_controller:get_requester_count().

set_size(Size) -> requester_pool_controller:set_requester_count(Size).

%%%===================================================================
%%% behaviour
%%%===================================================================

init(Size) ->
  folsom_metrics:new_counter(request),
  folsom_metrics:new_counter(retry),

  %folsom_metrics:new_histogram(test),
  %metrics_reader:register(test),
  metrics_observer:observe(request, request_histogram),

  Strategy = #{strategy => one_for_all, intensity => 1, period => 5},

  Specifications = [
    #{
      id => children,
      start => {requester_pool_children, start_link, []},
      type => supervisor
    },
    #{
      id => controller,
      start => {requester_pool_controller, start_link, [Size]},
      type => worker
    }
  ],

  {ok, {Strategy, Specifications}}.
