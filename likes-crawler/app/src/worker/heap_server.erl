-module(heap_server).

-behaviour(gen_server).

%%% api
-export([start_link/1, push/1, pull/1]).

%%% behaviour
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(MIN_HEAP_SIZE, 1000).
-define(INCREASE_HEAP_SIZE, 2000).

-record(state, {
  source,
  heap = [{filter_users, [[1052662]]}]
}).

%%%===================================================================
%%% api
%%%===================================================================

start_link(JobSource) -> gen_server:start_link({local, ?MODULE}, ?MODULE, JobSource, []).

push(Jobs) -> gen_server:cast(?MODULE, {push, Jobs}).

pull(JobCount) -> gen_server:call(?MODULE, {pull, JobCount}).

%%%===================================================================
%%% behaviour
%%%===================================================================

init(JobSource) ->
  NewState = #state{source = JobSource},
  {ok, NewState}.

%%%===================================================================
%%% pull
%%%===================================================================

handle_call({pull, JobCount}, _From, #state{heap = Heap, source = Source} = State) ->

  {PullHeap, NewHeap} = util:list_split(Heap, JobCount),

  case length(NewHeap) >= ?MIN_HEAP_SIZE of
    true ->
      NewState = State#state{heap = NewHeap},
      {reply, {ok, PullHeap}, NewState};

    false ->
      gen_server:reply(_From, {ok, PullHeap}),
      {ok, IncreaseHeap} = Source(?INCREASE_HEAP_SIZE),
      NewState = State#state{heap = NewHeap ++ IncreaseHeap},
      {noreply, NewState}
  end;

%%%===================================================================

handle_call(_Request, _From, State) -> {reply, ok, State}.

%%%===================================================================
%%% push
%%%===================================================================

handle_cast({push, Jobs}, #state{heap = Heap} = State) ->
  lager:debug("heap size: ~B", [length(Heap)]),

  NewHeap = Jobs ++ Heap,
  NewState = State#state{
    heap = NewHeap
  },
  {noreply, NewState};

%%%===================================================================

handle_cast(_Request, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
