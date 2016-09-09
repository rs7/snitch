-module(requester_server).

-behaviour(gen_server).

%%% api
-export([start_link/0, reserve/2, release/3]).

%%% behaviour
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
  heap = [],
  reserved = #{}
}).

%%%===================================================================
%%% api
%%%===================================================================

start_link() -> gen_server:start_link(?MODULE, [], []).

reserve(RequesterPid, Count) -> gen_server:call(RequesterPid, {reserve, Count}, infinity).

release(RequesterPid, RequestRef, Result) -> gen_server:cast(RequesterPid, {release, RequestRef, Result}).

%%%===================================================================
%%% behaviour
%%%===================================================================

init([]) -> {ok, #state{}}.

%%%===================================================================
%%% reserve
%%%===================================================================

handle_call({reserve, Count}, _From, #state{heap = Heap, reserved = Reserved} = State) ->

  AvailableRequests = case length(Heap) < Count of
    true ->
      {ok, NewRequests} = heap_server:reserve(),
      Heap ++ NewRequests;
    false -> Heap
  end,

  {ResultRequests, NewHeap} = util:list_split(AvailableRequests, Count),

  NewReserved = maps:merge(Reserved, maps:from_list(ResultRequests)),

  NewState = State#state{
    heap = NewHeap,
    reserved = NewReserved
  },
  {reply, {ok, ResultRequests}, NewState};

%%%===================================================================

handle_call(_Request, _From, State) -> {reply, ok, State}.

%%%===================================================================
%%% result
%%%===================================================================

%% TODO: группировать release-вызовы к heap_server

handle_cast({release, RequestRef, Result}, #state{reserved = Reserved} = State) ->
  {_RequestData, NewReserved} = maps:take(RequestRef, Reserved),

  heap_server:release(RequestRef, Result),

  NewState = State#state{reserved = NewReserved},
  {noreply, NewState};

%%%===================================================================

handle_cast(_Request, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
