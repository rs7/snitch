-module(requester_controller).

-behaviour(gen_server).

%%% api
-export([start_link/1, get/2, result/3, retry/3]).

%%% behaviour
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER_NAME(RequesterRef), {via, identifiable, {?MODULE, RequesterRef}}).

-record(state, {jobs = #{}}).

%%%===================================================================
%%% api
%%%===================================================================

start_link(RequesterRef) -> gen_server:start_link(?SERVER_NAME(RequesterRef), ?MODULE, [], []).

get(RequesterRef, RequestCount) -> gen_server:call(?SERVER_NAME(RequesterRef), {get, RequestCount}, infinity).

result(RequesterRef, RequestRef, Result) -> gen_server:cast(?SERVER_NAME(RequesterRef), {result, RequestRef, Result}).

retry(RequesterRef, RequestRef, Reason) -> gen_server:cast(?SERVER_NAME(RequesterRef), {retry, RequestRef, Reason}).

%%%===================================================================
%%% behaviour
%%%===================================================================

init([]) ->
  NewState = #state{},
  {ok, NewState}.

%%%===================================================================
%%% reserve
%%%===================================================================

handle_call(
  {get, Count}, _From,
  #state{jobs = Jobs} = State
) ->
  {ok, JobList} = task_manager:reserve(request, Count),

  RequestRefList = [make_ref() || _ <- JobList],
  RequestDataList = [request_data(JobData) || {_JobRef, JobData} <- JobList],

  RequestInfoList = lists:zip(RequestRefList, RequestDataList),

  NewJobs = maps:merge(Jobs, maps:from_list(lists:zip(RequestRefList, JobList))),

  NewState = State#state{jobs = NewJobs},
  {reply, {ok, RequestInfoList}, NewState};

%%%===================================================================

handle_call(_Request, _From, State) -> {reply, ok, State}.

%%%===================================================================
%%% release
%%%===================================================================

handle_cast({result, RequestRef, Result}, #state{jobs = Jobs} = State) ->
  {{JobRef, JobData}, NewJobs} = maps:take(RequestRef, Jobs),

  task_manager:release(JobRef, {ok, process_response(JobData, Result)}),

  NewState = State#state{jobs = NewJobs},
  {noreply, NewState};

handle_cast({retry, RequestRef, Reason}, #state{jobs = Jobs} = State) ->
  lager:warning("retry ~p", [Reason]),

  {{JobRef, _JobData}, NewJobs} = maps:take(RequestRef, Jobs),

  task_manager:release(JobRef, retrieve),

  NewState = State#state{jobs = NewJobs},
  {noreply, NewState};

%%%===================================================================

handle_cast(_Request, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%===================================================================
%%% internal
%%%===================================================================

request_data({Type, Args}) -> Type:request(Args).

process_response({Type, Args}, Result) -> Type:response(Result, Args).
