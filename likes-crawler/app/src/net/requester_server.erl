-module(requester_server).

-behaviour(gen_server).

%%% api
-export([start_link/0, reserve/2, release/3]).

%%% behaviour
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
  reserve = #{}
}).

%%%===================================================================
%%% api
%%%===================================================================

start_link() -> gen_server:start_link(?MODULE, [], []).

reserve(RequesterPid, RequestCount) -> gen_server:call(RequesterPid, {reserve, RequestCount}, infinity).

release(RequesterPid, RequestRef, Result) -> gen_server:cast(RequesterPid, {release, RequestRef, Result}).

%%%===================================================================
%%% behaviour
%%%===================================================================

init([]) -> {ok, #state{}}.

%%%===================================================================
%%% reserve
%%%===================================================================

handle_call({reserve, RequestCount}, _From, #state{reserve = Reserve} = State) ->

  {ok, ReserveJobs} = gen_heap:pull(RequestCount),

  RequestDataItems = [Type:request(Args) || {Type, Args} <- ReserveJobs],

  RequestRefs = [make_ref() || _ <- RequestDataItems],

  NewReserve = maps:from_list(lists:zip(RequestRefs, ReserveJobs)),

  RequestInfos = lists:zip(RequestRefs, RequestDataItems),

  NewState = State#state{
    reserve = maps:merge(Reserve, NewReserve)
  },
  {reply, {ok, RequestInfos}, NewState};

%%%===================================================================

handle_call(_Request, _From, State) -> {reply, ok, State}.

%%%===================================================================
%%% release
%%%===================================================================

handle_cast({release, RequestRef, {result, Result}}, #state{reserve = Reserve} = State) ->
  {{Type, Args}, NewReserve} = maps:take(RequestRef, Reserve),

  ResultJobs = Type:response(Result, Args),

  gen_heap:push(ResultJobs),

  NewState = State#state{
    reserve = NewReserve
  },
  {noreply, NewState};


handle_cast({release, RequestRef, {retry, Reason}}, #state{reserve = Reserve} = State) ->
  {RetryJob, NewReserve} = maps:take(RequestRef, Reserve),

  lager:warning("retry ~p ~p", [RetryJob, Reason]),

  gen_heap:push(RetryJob),

  NewState = State#state{
    reserve = NewReserve
  },
  {noreply, NewState};

%%%===================================================================

handle_cast(_Request, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
