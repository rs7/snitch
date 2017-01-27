-module(likes).

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
  Strategy = #{strategy => one_for_all},

  Specifications = [
    #{
      id => catalog_queue_controllers,
      start => {catalog_queue_controllers, start_link, []},
      type => supervisor
    }
  ],

  {ok, {Strategy, Specifications}}.
