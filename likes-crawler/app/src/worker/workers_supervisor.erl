-module(workers_supervisor).

-behaviour(supervisor).

%%% api
-export([start_link/0, set_workers_count/1]).

%%% behaviour
-export([init/1]).

%%%===================================================================
%%% api
%%%===================================================================

start_link() ->
  {ok, Pid} = supervisor:start_link({local, ?MODULE}, ?MODULE, []),
  set_workers_count(3),
  {ok, Pid}.

set_workers_count(Count) ->
  Pids = get_workers_pids(),
  set_workers_count(Count, length(Pids), Pids).

set_workers_count(Count, CurrentCount, _Pids) when Count =:= CurrentCount ->
  lager:debug("workers count set to ~B", [CurrentCount]),
  ok;

set_workers_count(Count, CurrentCount, _Pids) when Count >= CurrentCount ->
  {ok, _Pid} = supervisor:start_child(?MODULE, []),
  set_workers_count(Count, CurrentCount + 1, [_Pid | _Pids]);

set_workers_count(Count, CurrentCount, [Pid | Pids]) when Count =< CurrentCount ->
  {ok, _Pid} = supervisor:terminate_child(?MODULE, Pid),
  set_workers_count(Count, CurrentCount - 1, Pids).

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

get_workers_pids() -> [Pid || {worker, Pid, _, _} <- supervisor:which_children(?MODULE)].
