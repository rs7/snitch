-module(job_server).

-behaviour(gen_server).

%%% api
-export([start_link/3, get_ref/1]).

%%% behaviour
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("../../util/identified_name.hrl").

-define(SERVER_NAME(JobRef), ?IDENTIFIED_NAME(?MODULE, JobRef)).

-record(state, {controller_ref, job_ref, job_data, job_children}).

%%%===================================================================
%%% api
%%%===================================================================

start_link(ControllerRef, JobRef, JobData) ->
  gen_server:start_link(?SERVER_NAME(JobRef), ?MODULE, {ControllerRef, JobRef, JobData}, []).

get_ref(JobRef) -> ?SERVER_NAME(JobRef).

%%%===================================================================
%%% behaviour
%%%===================================================================

init({ControllerRef, JobRef, JobData}) ->
  self() ! process_job,
  NewState = #state{controller_ref = ControllerRef, job_ref = JobRef, job_data = JobData},
  {ok, NewState}.

handle_call(_Request, _From, State) -> {reply, ok, State}.

handle_cast(_Request, State) -> {noreply, State}.

handle_info(
  process_job, #state{controller_ref = ControllerRef, job_ref = JobRef, job_data = {JobType, JobContext}} = State
) ->
  {ok, ChildJobDataItems} = JobType:process(JobContext),

  case ChildJobDataItems of
    [] ->
      util:send(ControllerRef, {complete, JobRef}),
      {noreply, State};

    ChildJobDataItems ->
      ChildJobRefItems = start_child_jobs(JobRef, ChildJobDataItems),
      NewState = State#state{job_children = job_children_list:create(ChildJobRefItems)},
      {noreply, NewState}
  end;

handle_info(
  {complete, ChildJobRef}, #state{controller_ref = ControllerRef, job_ref = JobRef, job_children = JobChildren} = State
) ->
  ok = job:terminate_child_job(JobRef, ChildJobRef),

  NewJobChildren = job_children_list:remove(ChildJobRef, JobChildren),

  case job_children_list:is_empty(NewJobChildren) of
    true -> util:send(ControllerRef, {complete, JobRef});
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

start_child_jobs(JobRef, ChildJobDataItems) -> start_child_jobs(JobRef, ChildJobDataItems, []).

start_child_jobs(_JobRef, [], ChildJobRefItems) -> ChildJobRefItems;
start_child_jobs(JobRef, [ChildJobData | RemainingChildJobDataItems], ChildJobRefItems) ->
  ChildJobRef = start_child_job(JobRef, ChildJobData),
  start_child_jobs(JobRef, RemainingChildJobDataItems, [ChildJobRef | ChildJobRefItems]).

start_child_job(JobRef, ChildJobData) ->
  ChildJobRef = make_ref(),
  {ok, _ChildJobPid} = job:start_child_job(JobRef, ChildJobRef, ChildJobData),
  ChildJobRef.

