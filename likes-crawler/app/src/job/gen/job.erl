-module(job).

-behaviour(supervisor).

%%% api
-export([start_link/4, stop/1, start_child_job/4, terminate_child_job/2]).

%%% behaviour
-export([init/1]).

-include("../../util/identified_name.hrl").

-define(SERVER_NAME(JobRef), ?IDENTIFIED_NAME(?MODULE, JobRef)).

%%%===================================================================
%%% api
%%%===================================================================

start_link(ControllerRef, JobPriority, JobRef, JobData) ->
  supervisor:start_link(?SERVER_NAME(JobRef), ?MODULE, {ControllerRef, JobPriority, JobRef, JobData}).

stop(JobRef) -> exit(whereis(?SERVER_NAME(JobRef)), shutdown). %todo: необходим ли этот метод?

start_child_job(JobRef, ChildJobPriority, ChildJobRef, ChildJobData) ->
  ControllerRef = job_server:get_ref(JobRef),
  job_children:start_child(JobRef, ControllerRef, ChildJobPriority, ChildJobRef, ChildJobData).

terminate_child_job(JobRef, ChildJobRef) -> job_children:terminate_child(JobRef, ?IDENTIFIED_PID(?MODULE, ChildJobRef)).

%%%===================================================================
%%% behaviour
%%%===================================================================

init({ControllerRef, JobPriority, JobRef, JobData}) ->
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
      start => {job_server, start_link, [ControllerRef, JobPriority, JobRef, JobData]},
      type => worker
    }
  ],

  {ok, {Strategy, Specifications}}.
