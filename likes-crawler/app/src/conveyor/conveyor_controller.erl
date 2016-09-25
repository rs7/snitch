-module(conveyor_controller).

-behaviour(gen_server).

%%% api
-export([start_link/1, is_target_user/1]).

%%% behaviour
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(START_JOB_MESSAGE, start_job).
-define(USERS_COUNT, 1000).

-record(state, {list_ref, last_user = 0}).

%%%===================================================================
%%% api
%%%===================================================================

start_link(ListRef) -> gen_server:start_link({local, ?MODULE}, ?MODULE, ListRef, []).

is_target_user(Liker) -> lists:member(Liker, [53083705, 38940203, 41362423]).

%%%===================================================================
%%% behaviour
%%%===================================================================

init(ListRef) ->
  self() ! ?START_JOB_MESSAGE,
  self() ! ?START_JOB_MESSAGE,
  self() ! ?START_JOB_MESSAGE,
  self() ! ?START_JOB_MESSAGE,
  self() ! ?START_JOB_MESSAGE,
  NewState = #state{list_ref = ListRef},
  {ok, NewState}.

handle_call(_Request, _From, State) -> {reply, ok, State}.

handle_cast(_Request, State) -> {noreply, State}.

handle_info(?START_JOB_MESSAGE, #state{list_ref = ListRef, last_user = LastUser} = State) ->
  NewLastUser = LastUser + ?USERS_COUNT,
  Users = lists:seq(LastUser + 1, NewLastUser),
  JobRef= make_ref(),
  JobPriority = [LastUser + 1],
  JobBody = {request_job, {filter_users, Users}},
  job_list:start_job(ListRef, JobRef, JobPriority, JobBody, self()),
  lager:info("JOB STARTED ~p ~p", [JobRef, JobBody]),
  NewState = State#state{last_user = NewLastUser},
  {noreply, NewState};

handle_info({complete, JobRef}, State) ->
  lager:info("JOB COMPLETE ~p", [JobRef]),
  self() ! ?START_JOB_MESSAGE,
  {noreply, State};

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
