-module(requester_controller).

-behaviour(gen_server).

%%% api
-export([start_link/1, get/2, result/3, retry/3]).

%%% behaviour
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER_NAME(RequesterRef), {via, identifiable, {?MODULE, RequesterRef}}).

-record(state, {jobs = #{}, requests = #{}, cache = []}).

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
  #state{jobs = Jobs, requests = Requests, cache = Cache} = State
) ->
  {Cached, NewCache} = util:list_split(Cache, Count),

  Reserved = task_manager:reserve(request, Count - length(Cached)),

  JobList = lists:append(Cached, Reserved),

  CreatedRequests = [create_request(JobData) || {_JobRef, JobData} <- JobList],

  NewJobs = maps:merge(Jobs, maps:from_list(Reserved)),
  NewRequests = maps:merge(Requests, maps:from_list(CreatedRequests)),

  NewState = State#state{jobs = NewJobs, requests = NewRequests, cache = NewCache},
  {reply, {ok, CreatedRequests}, NewState};

%%%===================================================================

handle_call(_Request, _From, State) -> {reply, ok, State}.

%%%===================================================================
%%% release
%%%===================================================================

handle_cast({result, RequestRef, Result}, #state{jobs = Jobs} = State) ->
  {{JobRef, JobData}, NewJobs} = maps:take(RequestRef, Jobs),

  task_manager:release(JobRef, process_response(JobData, Result)),

  NewState = State#state{jobs = NewJobs},
  {noreply, NewState};

handle_cast({retry, RequestRef, Reason}, #state{jobs = Jobs, cache = Cache} = State) ->
  lager:warning("retry ~p", [Reason]),

  {{JobRef, JobData}, NewJobs} = maps:take(RequestRef, Jobs),

  NewCache = [{JobRef, JobData}] ++ Cache,

  NewState = State#state{jobs = NewJobs, cache = NewCache},
  {noreply, NewState};

%%%===================================================================

handle_cast(_Request, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%===================================================================
%%% internal
%%%===================================================================

create_request({Type, Args}) ->
  RequestRef = make_ref(),
  RequestData = Type:request(Args),
  {RequestRef, RequestData}.

process_response({Type, Args}, Result) -> Type:response(Result, Args).
