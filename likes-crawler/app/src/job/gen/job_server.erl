-module(job_server).

-behaviour(gen_server).

%%% api
-export([start_link/4, get_ref/1]).

%%% behaviour
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("../../util/identified_name.hrl").

-define(SERVER_NAME(JobRef), ?IDENTIFIED_NAME(?MODULE, JobRef)).

-record(state, {controller_ref, job_ref, job_data, job_children, job_priority}).

%%%===================================================================
%%% api
%%%===================================================================

start_link(ControllerRef, JobPriority, JobRef, JobData) ->
  gen_server:start_link(?SERVER_NAME(JobRef), ?MODULE, {ControllerRef, JobPriority, JobRef, JobData}, []).

get_ref(JobRef) -> ?SERVER_NAME(JobRef).

%%%===================================================================
%%% behaviour
%%%===================================================================

init({ControllerRef, JobPriority, JobRef, JobData}) ->
  self() ! process_job,
  NewState = #state{controller_ref = ControllerRef, job_ref = JobRef, job_data = JobData, job_priority = JobPriority},
  {ok, NewState}.

handle_call(_Request, _From, State) -> {reply, ok, State}.

handle_cast(_Request, State) -> {noreply, State}.

handle_info(
  process_job,
  #state{
    controller_ref = ControllerRef, job_ref = JobRef, job_data = {JobType, JobContext}, job_priority = JobPriority
  } = State
) ->
  %lager:info("process_job ~p", [{JobPriority, JobType, JobContext}]),

  {ok, ChildJobDataItems} = JobType:process(JobPriority, JobContext),

  case ChildJobDataItems of
    [] ->
      complete(ControllerRef, JobRef),
      {noreply, State};

    ChildJobDataItems ->
      ChildJobRefItems = start_child_jobs(JobRef, JobPriority, ChildJobDataItems),
      NewState = State#state{job_children = job_children_list:create(ChildJobRefItems)},
      {noreply, NewState}
  end;

handle_info(
  {complete, ChildJobRef}, #state{controller_ref = ControllerRef, job_ref = JobRef, job_children = JobChildren} = State
) ->
  ok = job:terminate_child_job(JobRef, ChildJobRef),

  NewJobChildren = job_children_list:remove(ChildJobRef, JobChildren),

  case job_children_list:is_empty(NewJobChildren) of
    true -> complete(ControllerRef, JobRef);
    false -> ok
  end,

  NewState = State#state{job_children = NewJobChildren},
  {noreply, NewState};

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%===================================================================
%%% internal
%%%===================================================================

start_child_jobs(JobRef, JobPriority, ChildJobDataItems) ->
  start_child_jobs(JobRef, JobPriority, ChildJobDataItems, [], 1).

start_child_jobs(_JobRef, _JobPriority, [], ChildJobRefItems, _ChildJobNumber) -> ChildJobRefItems;
start_child_jobs(JobRef, JobPriority, [ChildJobData | RemainingChildJobDataItems], ChildJobRefItems, ChildJobNumber) ->
  ChildJobPriority = JobPriority ++ [ChildJobNumber],
  ChildJobRef = start_child_job(JobRef, ChildJobPriority, ChildJobData),
  start_child_jobs(
    JobRef, JobPriority, RemainingChildJobDataItems, [ChildJobRef | ChildJobRefItems], ChildJobNumber + 1
  ).

start_child_job(JobRef, ChildJobPriority, ChildJobData) ->
  ChildJobRef = make_ref(),
  {ok, _ChildJobPid} = job:start_child_job(JobRef, ChildJobPriority, ChildJobRef, ChildJobData),
  ChildJobRef.

complete(ControllerRef, JobRef) -> util:send(ControllerRef, {complete, JobRef}).
