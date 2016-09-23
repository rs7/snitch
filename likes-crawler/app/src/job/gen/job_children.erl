-module(job_children).

-behaviour(supervisor).

%%% api
-export([start_link/1, start_child/5, terminate_child/2]).

%%% behaviour
-export([init/1]).

-include("../../util/identified_name.hrl").

-define(SERVER_NAME(JobRef), ?IDENTIFIED_NAME(?MODULE, JobRef)).

%%%===================================================================
%%% api
%%%===================================================================

start_link(JobRef) -> supervisor:start_link(?SERVER_NAME(JobRef), ?MODULE, []).

start_child(JobRef, ControllerRef, ChildJobPriority, ChildJobRef, ChildJobData) ->
  supervisor:start_child(?SERVER_NAME(JobRef), [ControllerRef, ChildJobPriority, ChildJobRef, ChildJobData]).

terminate_child(JobRef, ChildJobPid) -> supervisor:terminate_child(?SERVER_NAME(JobRef), ChildJobPid).

%%%===================================================================
%%% behaviour
%%%===================================================================

init([]) ->
  Strategy = #{strategy => simple_one_for_one, intensity => 1, period => 5},

  Specifications = [
    #{
      id => child_job,
      restart => transient,
      start => {job, start_link, []},
      type => worker
    }
  ],

  {ok, {Strategy, Specifications}}.
