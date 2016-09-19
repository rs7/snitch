-module(gen_heap).

-behaviour(gen_server).

%%% api
-export([behaviour_info/1, start_link/2, start_link/3, push/2, pull/2, get_size/1]).

%%% behaviour
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
  heap = [],
  sizes,
  behaviour_module,
  behaviour_state
}).

%%%===================================================================
%%% api
%%%===================================================================

behaviour_info(callbacks) -> [{init, 1}, {pull_from_up_level, 2}, {push_to_up_level, 2}];

behaviour_info(_Other) -> undefined.

start_link(Module, Args) -> gen_server:start_link(?MODULE, [Module, Args], []).

start_link(HeapName, Module, Args) -> gen_server:start_link(HeapName, ?MODULE, [Module, Args], []).

pull(HeapRef, Count) -> gen_server:call(HeapRef, {pull, Count}, infinity).

push(HeapRef, Items) -> gen_server:cast(HeapRef, {push, Items}).

get_size(HeapRef) -> gen_server:call(HeapRef, get_size).

%%%===================================================================
%%% behaviour
%%%===================================================================

init([BehaviourModule, Args]) ->
  {ok, Sizes, BehaviourState} = BehaviourModule:init(Args),
  NewState = #state{
    sizes = Sizes,
    behaviour_module = BehaviourModule,
    behaviour_state = BehaviourState
  },
  {ok, NewState}.

%%%===================================================================
%%% pull
%%%===================================================================

handle_call(
  {pull, Count}, From,
  #state{heap = Heap, sizes = Sizes, behaviour_module = BehaviourModule, behaviour_state = BehaviourState} = State
) ->
  {Items, CurrentHeap} = util:list_split(Heap, Count),

  gen_server:reply(From, {ok, Items}),

  {NewHeap, NewBehaviourState} = normalize_heap(CurrentHeap, Sizes, {BehaviourModule, BehaviourState}),

  NewState = State#state{heap = NewHeap, behaviour_state = NewBehaviourState},

  {noreply, NewState};

%%%===================================================================
%%% get_size
%%%===================================================================

handle_call(get_size, _From, #state{heap = Heap} = State) ->
  Result = length(Heap),
  {reply, {ok, Result}, State};

%%%===================================================================

handle_call(_Request, _From, State) -> {reply, ok, State}.

%%%===================================================================
%%% push
%%%===================================================================

handle_cast(
  {push, Items},
  #state{heap = Heap, sizes = Sizes, behaviour_module = BehaviourModule, behaviour_state = BehaviourState} = State
) ->
  CurrentHeap = Items ++ Heap,

  {NewHeap, NewBehaviourState} = normalize_heap(CurrentHeap, Sizes, {BehaviourModule, BehaviourState}),

  NewState = State#state{heap = NewHeap, behaviour_state = NewBehaviourState},

  {noreply, NewState};

%%%===================================================================

handle_cast(_Request, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, #state{heap = Heap, behaviour_module = BehaviourModule, behaviour_state = BehaviourState}) ->
  {ok, _NewBehaviourState} = BehaviourModule:push_to_up_level(Heap, BehaviourState),
  ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%===================================================================
%%% internal
%%%===================================================================

normalize_heap(Heap, {_MinSize, _NormalSize, _MaxSize} = Sizes, Behaviour) ->
  normalize_heap(Heap, check_size(length(Heap), Sizes), Behaviour);

normalize_heap(Heap, normal, {_BehaviourModule, BehaviourState}) -> {Heap, BehaviourState};

normalize_heap(Heap, {add, Size}, {BehaviourModule, BehaviourState}) ->
  {ok, AddHeap, NewBehaviourState} = BehaviourModule:pull_from_up_level(Size, BehaviourState),
  NewHeap = AddHeap ++ Heap,
  {NewHeap, NewBehaviourState};

normalize_heap(Heap, {subtract, Size}, {BehaviourModule, BehaviourState}) ->
  {NewHeap, SubtractPart} = lists:split(length(Heap) - Size, Heap),
  {ok, NewBehaviourState} = BehaviourModule:push_to_up_level(SubtractPart, BehaviourState),
  {NewHeap, NewBehaviourState}.

check_size(Current, {Min, Normal, _Max}) when Current < Min -> {add, Normal - Current};
check_size(Current, {_Min, Normal, Max}) when Current > Max -> {subtract, Current - Normal};
check_size(_Current, _Sizes) -> normal.
