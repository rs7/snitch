-module(workers_pool_supervisor).

-behaviour(supervisor).

%% api
-export([start_link/0, start_child/0]).

%% supervisor
-export([init/1]).

%%====================================================================
%% api
%%====================================================================

start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child() -> supervisor:start_child(?MODULE, []).

%%====================================================================
%% supervisor
%%====================================================================

init([]) ->
  Strategy = #{strategy => simple_one_for_one, intensity => 10, period => 1},
  ChildSpecification = #{
    id => worker,
    start => {worker_supervisor, start_link, []},
    type => supervisor
  },
  {ok, {Strategy, [ChildSpecification]}}.
