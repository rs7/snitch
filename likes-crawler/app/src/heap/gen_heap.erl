-module(gen_heap).

-behaviour(gen_server).

%%% api
-export([behaviour_info/1, start_link/3, push/2, pull/2]).

%%% behaviour
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
  min_size,
  max_size,
  normal_size,
  heap = [],
  behaviour_state,
  behaviour_module
}).

%%%===================================================================
%%% api
%%%===================================================================

behaviour_info(callbacks) ->
  [
    {init, 1},
    {pull, 2},
    {push, 2}
  ];

behaviour_info(_Other) -> undefined.

start_link(Name, Module, Args) -> gen_server:start_link(Name, ?MODULE, [Module, Args], []).

push(Name, Items) -> gen_server:cast(Name, {push, Items}).

pull(Name, Count) -> gen_server:call(Name, {pull, Count}).

%%%===================================================================
%%% behaviour
%%%===================================================================

init([BehaviourModule, Args]) ->
  {ok, MinSize, MaxSize, NormalSize, BehaviourState} = BehaviourModule:init(Args),
  NewState = #state{
    min_size = MinSize,
    max_size = MaxSize,
    normal_size = NormalSize,
    behaviour_state = BehaviourState,
    behaviour_module = BehaviourModule
  },
  {ok, NewState}.

%%%===================================================================
%%% pull
%%%===================================================================

handle_call(
  {pull, Count}, From,
  #state{
    heap = Heap0, min_size = MinSize, normal_size = NormalSize,
    behaviour_module = BehaviourModule, behaviour_state = BehaviourState0
  } = State
) ->

  Size0 = length(Heap0),

  case Size0 >= Count of
    true ->
      Heap1 = Heap0,
      BehaviourState1 = BehaviourState0;

    false ->
      {Heap1, BehaviourState1} = increase_on(Heap0, NormalSize + Count - Size0, BehaviourModule, BehaviourState0)
  end,

  {Items, Heap2} = util:list_split(Heap1, Count),

  gen_server:reply(From, {ok, Items}),

  Size2 = length(Heap2),

  case Size2 >= MinSize of
    true ->
      Heap3 = Heap2,
      BehaviourState2 = BehaviourState1;

    false ->
      {Heap3, BehaviourState2} = increase_on(Heap2, NormalSize - Size2, BehaviourModule, BehaviourState1)
  end,

  NewState = State#state{heap = Heap3, behaviour_state = BehaviourState2},

  io:format("HEAP ~p SIZE ~B~n", [Heap3, length(Heap3)]),

  {noreply, NewState};

%%%===================================================================

handle_call(_Request, _From, State) -> {reply, ok, State}.

%%%===================================================================
%%% push
%%%===================================================================

handle_cast(
  {push, Items},
  #state{
    heap = Heap, max_size = MaxSize, normal_size = NormalSize,
    behaviour_module = BehaviourModule, behaviour_state = BehaviourState0
  } = State
) ->

  Heap1 = Items ++ Heap,

  Size1 = length(Heap1),

  case Size1 =< MaxSize of
    true ->
      Heap2 = Heap1,
      BehaviourState1 = BehaviourState0;

    false ->
      {Heap2, BehaviourState1} = decrease_on(Heap1, Size1 - NormalSize, BehaviourModule, BehaviourState0)
  end,

  NewState = State#state{heap = Heap2, behaviour_state = BehaviourState1},

  io:format("HEAP ~p SIZE ~B~n", [Heap2, length(Heap2)]),

  {noreply, NewState};

%%%===================================================================

handle_cast(_Request, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%===================================================================
%%% internal
%%%===================================================================

increase_on(Heap, Size, BehaviourModule, BehaviourState) ->
  {ok, HeapPart, NewBehaviourState} = BehaviourModule:pull(Size, BehaviourState),
  NewHeap = HeapPart ++ Heap,
  {NewHeap, NewBehaviourState}.

decrease_on(Heap, Size, BehaviourModule, BehaviourState) ->
  {HeapPart, NewHeap} = lists:split(Size, Heap),
  {ok, NewBehaviourState} = BehaviourModule:push(HeapPart, BehaviourState),
  {NewHeap, NewBehaviourState}.
