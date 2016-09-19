-module(requester).

-behaviour(gen_server).

%%% api
-export([start_link/0, set_coworkers/2, reserve/2, release/3]).

%%% behaviour
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
  heap_ref,
  reserve = #{}
}).

%%%===================================================================
%%% api
%%%===================================================================

start_link() -> gen_server:start_link(?MODULE, [], []).

set_coworkers(RequesterPid, Coworkers) -> gen_server:call(RequesterPid, {set_coworkers, Coworkers}).

reserve(RequesterPid, RequestCount) -> gen_server:call(RequesterPid, {reserve, RequestCount}, infinity).

release(RequesterPid, RequestRef, Result) -> gen_server:cast(RequesterPid, {release, RequestRef, Result}).

%%%===================================================================
%%% behaviour
%%%===================================================================

init([]) -> {ok, #state{}}.

%%%===================================================================
%%% set_coworkers
%%%===================================================================

handle_call({set_coworkers, [HeapRef]}, _From, State) ->
  NewState = State#state{heap_ref = HeapRef},
  {reply, ok, NewState};

%%%===================================================================
%%% reserve
%%%===================================================================

handle_call({reserve, RequestCount}, _From, #state{heap_ref = HeapRef, reserve = Reserve} = State) ->

  {ok, ReserveJobs} = gen_heap:pull(HeapRef, RequestCount),

  RequestDataItems = [Type:request(Context) || {Type, Context} <- ReserveJobs],

  RequestRefs = [make_ref() || _ <- RequestDataItems],

  RequestInfos = lists:zip(RequestRefs, RequestDataItems),

  NewReserve = maps:merge(Reserve, maps:from_list(lists:zip(RequestRefs, ReserveJobs))),

  NewState = State#state{reserve = NewReserve},
  {reply, {ok, RequestInfos}, NewState};

%%%===================================================================

handle_call(_Request, _From, State) -> {reply, ok, State}.

%%%===================================================================
%%% release
%%%===================================================================

handle_cast({release, RequestRef, {result, Result}}, #state{heap_ref = HeapRef, reserve = Reserve} = State) ->
  {{Type, Context}, NewReserve} = maps:take(RequestRef, Reserve),

  spawn_link(
    fun() ->
      %lager:debug("~p", [{Type, Context}]),
      gen_heap:push(HeapRef, Type:response(Result, Context))
    end
  ),

  NewState = State#state{reserve = NewReserve},
  {noreply, NewState};

handle_cast({release, RequestRef, {retry, Reason}}, #state{heap_ref = HeapRef, reserve = Reserve} = State) ->
  {RetryJob, NewReserve} = maps:take(RequestRef, Reserve),

  lager:warning("retry ~p ~p", [Reason, RetryJob]),

  gen_heap:push(HeapRef, [RetryJob]),

  NewState = State#state{reserve = NewReserve},
  {noreply, NewState};

%%%===================================================================

handle_cast(_Request, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
