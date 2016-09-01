-module(worker_supervisor).

-behaviour(supervisor).

%%% api
-export([start_link/1]).

%%% behaviour
-export([init/1]).

%%%===================================================================
%%% api
%%%===================================================================

start_link(Id) -> supervisor:start_link(?MODULE, Id).

%%%===================================================================
%%% behaviour
%%%===================================================================

init(Id) ->
  Strategy = #{strategy => one_for_all, intensity => 0, period => 1},

  Specifications = [
    #{
      id => connection,
      start => {connection_server, start_link, [Id]},
      type => worker
    },
    #{
      id => requester,
      start => {requester_server, start_link, [Id]},
      type => worker
    }
  ],

  {ok, {Strategy, Specifications}}.
