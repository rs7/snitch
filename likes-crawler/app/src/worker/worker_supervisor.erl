-module(worker_supervisor).

-behaviour(supervisor).

%%% api
-export([start_link/0]).

%%% behaviour
-export([init/1]).

%%%===================================================================
%%% api
%%%===================================================================

start_link() ->
  {ok, SupervisorPid} = supervisor:start_link(?MODULE, []),

  [
    {connection, ConnectionPid, _, _},
    {requester, RequesterPid, _, _},
    {logic, LogicPid, _, _}
  ] = supervisor:which_children(SupervisorPid),

  ok = logic_server:set_coworkers(LogicPid, [RequesterPid]),
  ok = connection_server:set_coworkers(ConnectionPid, [RequesterPid]),

  {ok, SupervisorPid}.

%%%===================================================================
%%% behaviour
%%%===================================================================

init([]) ->
  Strategy = #{strategy => one_for_all, intensity => 0, period => 1},

  Specifications = [
    #{
      id => logic,
      start => {logic_server, start_link, []},
      type => worker
    },
    #{
      id => requester,
      start => {requester_server, start_link, []},
      type => worker
    },
    #{
      id => connection,
      start => {connection_server, start_link, []},
      type => worker
    }
  ],

  {ok, {Strategy, Specifications}}.
