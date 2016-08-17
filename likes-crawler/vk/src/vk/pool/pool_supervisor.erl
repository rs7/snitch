-module(pool_supervisor).

-behaviour(supervisor).

%%% api
-export([start_link/0, start_worker/1]).

%%% behaviour
-export([init/1]).

%%%===================================================================
%%% api
%%%===================================================================

start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_worker(Id) -> supervisor:start_child(?MODULE, [Id]).

%%%===================================================================
%%% behaviour
%%%===================================================================

init([]) ->
  Strategy = #{strategy => simple_one_for_one, intensity => 10, period => 1},

  ChildSpecification = #{
    id => worker,
    start => {worker, start_link, []},
    type => supervisor
  },

  {ok, {Strategy, [ChildSpecification]}}.
