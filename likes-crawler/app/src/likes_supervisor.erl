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

  Specifications = [
    #{
      id => root_heap,
      start => {root_heap_server, start_link, []},
      type => worker
    },
    #{
      id => workers,
      start => {workers_supervisor, start_link, []},
      type => supervisor
    }
  ],

  {ok, {Strategy, Specifications}}.
