-module(request_executor).

-behaviour(application).
-behaviour(supervisor).

%%% behaviour application
-export([start/2, stop/1]).

%%% behaviour supervisor
-export([init/1]).

%%%===================================================================
%%% behaviour application
%%%===================================================================

start(_StartType, _StartArgs) -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

stop(_State) -> ok.

%%%===================================================================
%%% behaviour supervisor
%%%===================================================================

init([]) ->
  Strategy = #{strategy => one_for_all, intensity => 5, period => 1},

  {ok, RequesterCount} = application:get_env(requester_count),

  Specifications = [
    #{
      id => pool,
      start => {requester_pool, start_link, [RequesterCount]},
      type => supervisor
    },
    #{
      id => test,
      start => {test, start_link, []},
      type => worker
    }
  ],

  {ok, {Strategy, Specifications}}.
