-module(root_supervisor).

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
  Strategy = #{strategy => one_for_all, intensity => 1, period => 5},

  Specifications = [
    #{
      id => pool,
      start => {pool, start_link, []},
      type => worker
    },
    #{
      id => users,
      start => {users, start_link, []},
      type => worker
    }
  ],

  {ok, {Strategy, Specifications}}.
