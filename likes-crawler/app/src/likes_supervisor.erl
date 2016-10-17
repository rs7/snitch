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

  Specifications = [
    #{
      id => requester_pool,
      start => {requester_pool, start_link, [RequesterCount]},
      type => worker
    }
  ],

  {ok, {Strategy, Specifications}}.
