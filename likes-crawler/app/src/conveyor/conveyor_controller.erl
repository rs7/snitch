-module(conveyor_controller).

-behaviour(gen_server).

%%% api
-export([start_link/2, is_target_user/1]).

%%% behaviour
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(START_JOB_MESSAGE, start_job).

-record(state, {list_ref, conveyor_list, next_job_number}).

%%%===================================================================
%%% api
%%%===================================================================

start_link(ListRef, JobsInOneTime) -> gen_server:start_link({local, ?MODULE}, ?MODULE, {ListRef, JobsInOneTime}, []).

is_target_user(Liker) -> lists:member(Liker, [53083705, 38940203, 41362423, 1052662]).

%%%===================================================================
%%% behaviour
%%%===================================================================

init({ListRef, JobsInOneTime}) ->
  start_jobs(JobsInOneTime),
  NewConveyorList = conveyor_list:create_list(),
  NextJobNumber = 1,
  NewState = #state{list_ref = ListRef, conveyor_list = NewConveyorList, next_job_number = NextJobNumber},
  {ok, NewState}.

handle_call(_Request, _From, State) -> {reply, ok, State}.

handle_cast(_Request, State) -> {noreply, State}.

handle_info(?START_JOB_MESSAGE, #state{conveyor_list = undefined} = State) -> {noreply, State};

handle_info(
  ?START_JOB_MESSAGE,
  #state{list_ref = ListRef, conveyor_list = ConveyorList, next_job_number = NextJobNumber} = State
) ->
  {Users, NewConveyorList} = conveyor_list:extract(ConveyorList),

  JobRef = make_ref(),
  JobPriority = [NextJobNumber],
  JobBody = {get_users, Users},
  job_list:start_job(ListRef, JobRef, JobPriority, JobBody, self()),

  lager:info("JOB START ~p", [NextJobNumber]),

  NewNextJobNumber = NextJobNumber + 1,
  NewState = State#state{conveyor_list = NewConveyorList, next_job_number = NewNextJobNumber},
  {noreply, NewState};

handle_info({complete, _JobRef}, State) ->
  self() ! ?START_JOB_MESSAGE,
  {noreply, State};

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%===================================================================
%%% internal
%%%===================================================================

start_jobs(0) -> ok;
start_jobs(Count) ->
  self() ! ?START_JOB_MESSAGE,
  start_jobs(Count - 1).
