-module(job).

-behaviour(supervisor).

%%% api
-export([start_link/3, stop/1, start_child_job/3, terminate_child_job/2]).

%%% behaviour
-export([init/1]).

-include("../../util/identified_name.hrl").

-define(SERVER_NAME(JobRef), ?IDENTIFIED_NAME(?MODULE, JobRef)).

%%%===================================================================
%%% api
%%%===================================================================

start_link(ControllerRef, JobRef, JobData) ->
  supervisor:start_link(?SERVER_NAME(JobRef), ?MODULE, {ControllerRef, JobRef, JobData}).

stop(JobRef) -> exit(whereis(?SERVER_NAME(JobRef)), shutdown).

start_child_job(JobRef, ChildJobRef, ChildJobData) ->
  ControllerRef = job_server:get_ref(JobRef),
  job_children:start_child(JobRef, ControllerRef, ChildJobRef, ChildJobData).

terminate_child_job(JobRef, ChildJobRef) ->
  ChildJobPid = gproc:lookup_pid(?SERVER_NAME(ChildJobRef)),
  job_children:terminate_child(JobRef, ChildJobPid).

%%%===================================================================
%%% behaviour
%%%===================================================================

init({ControllerRef, JobRef, JobData}) ->
  Strategy = #{strategy => one_for_all, intensity => 1, period => 5},

  Specifications = [
    #{
      id => children,
      restart => transient,
      start => {job_children, start_link, [JobRef]},
      type => supervisor
    },
    #{
      id => server,
      restart => transient,
      start => {job_server, start_link, [ControllerRef, JobRef, JobData]},
      type => worker
    }
  ],

  {ok, {Strategy, Specifications}}.
