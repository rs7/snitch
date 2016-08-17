-module(worker).

-behaviour(supervisor).

%%% api
-export([start_link/1]).

%%% behaviour
-export([init/1]).

%%%===================================================================
%%% api
%%%===================================================================

start_link(WorkerId) -> supervisor:start_link(?MODULE, WorkerId).

%%%===================================================================
%%% behaviour
%%%===================================================================

init(WorkerId) ->
  Strategy = #{strategy => one_for_one, intensity => 5, period => 1},

  Specifications = [
    #{
      id => requester,
      start => {requester, start_link, [WorkerId]},
      type => worker
    },
    #{
      id => connection,
      start => {requester, open_connection, [WorkerId]},
      type => worker
    },
    #{
      id => user_process,
      start => {user_process, start_link, [WorkerId]},
      type => worker
    }
  ],

  {ok, {Strategy, Specifications}}.
