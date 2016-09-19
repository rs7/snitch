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
    {heap, HeapPid, _, _}
  ] = supervisor:which_children(SupervisorPid),

  ok = requester:set_coworkers(RequesterPid, [HeapPid]),
  ok = connection:set_coworkers(ConnectionPid, [RequesterPid]),

  {ok, SupervisorPid}.

%%%===================================================================
%%% behaviour
%%%===================================================================

init([]) ->
  Strategy = #{strategy => one_for_all, intensity => 0, period => 1},

  Specifications = [
    #{
      id => heap,
      start => {heap, start_link, [local_heap, {100, 300, 600}]},
      type => worker
    },
    #{
      id => requester,
      start => {requester, start_link, []},
      type => worker
    },
    #{
      id => connection,
      start => {connection, start_link, []},
      type => worker
    }
  ],

  {ok, {Strategy, Specifications}}.
