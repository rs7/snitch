-module(workers_supervisor).

-behaviour(supervisor).

%%% api
-export([start_link/0]).

%%% behaviour
-export([init/1]).

%%%===================================================================
%%% api
%%%===================================================================

start_link() ->
  {ok, SupervisorPid} = supervisor:start_link({local, ?MODULE}, ?MODULE, []),
  start_children(SupervisorPid, 10),
  {ok, SupervisorPid}.

%%%===================================================================
%%% behaviour
%%%===================================================================

init([]) ->
  Strategy = #{strategy => simple_one_for_one, intensity => 0, period => 1},

  Specifications = [
    #{
      id => worker,
      start => {worker_supervisor, start_link, []},
      type => worker
    }
  ],

  {ok, {Strategy, Specifications}}.

%%%===================================================================
%%% internal
%%%===================================================================

start_children(_SupervisorPid, 0) -> ok;

start_children(SupervisorPid, N) ->
  {ok, _Pid} = supervisor:start_child(SupervisorPid, []),
  start_children(SupervisorPid, N - 1).
