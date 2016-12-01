-module(metrics).

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
  Strategy = #{strategy => one_for_all},

  Specifications = [
    #{
      id => data,
      start => {metrics_data, start_link, []}
    },
    #{
      id => info,
      start => {metrics_info, start_link, []}
    }
  ],

  {ok, {Strategy, Specifications}}.
