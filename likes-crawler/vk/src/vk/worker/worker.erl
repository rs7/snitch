-module(worker).

-behaviour(supervisor).

%%% api
-export([start_link/2]).

%%% behaviour
-export([init/1]).

%%%===================================================================
%%% api
%%%===================================================================

start_link(Process, WorkerId) -> supervisor:start_link(?MODULE, {WorkerId, Process}).

%%%===================================================================
%%% behaviour
%%%===================================================================

init({WorkerId, Process}) ->
  Strategy = #{strategy => rest_for_one, intensity => 5, period => 1},

  Specifications = [
    #{
      id => process,
      start => {process, start_link, [WorkerId, Process]},
      type => worker
    },
    #{
      id => requester,
      start => {requester, start_link, [WorkerId]},
      type => worker
    },
    #{
      id => connection,
      start => {requester, open_connection, [WorkerId]},
      type => worker
    }
  ],

  {ok, {Strategy, Specifications}}.
