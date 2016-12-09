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
      id => metrics,
      start => {metrics, start_link, []}
    },
    #{
      id => request_rpc,
      start => {request_rpc, start_link, []}
    },
    #{
      id => requesters_pool,
      start => {requesters_pool, start_link, []}
    },
    #{
      id => user_pool,
      start => {user_pool, start_link, []}
    }
  ],

  {ok, {Strategy, Specifications}}.
