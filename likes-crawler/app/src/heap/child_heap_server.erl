-module(child_heap_server).

-behaviour(gen_heap).

%%% api
-export([start_link/2, pull/2, push/2]).

%%% behaviour
-export([init/1, pull_up/2, push_up/2]).

-record(state, {parent}).

%%%===================================================================
%%% api
%%%===================================================================

start_link(ParentHeap, {MinSize, NormalSize, MaxSize}) ->
  gen_heap:start_link(?MODULE, [ParentHeap, {MinSize, NormalSize, MaxSize}]).

pull(Heap, Count) -> gen_heap:pull(Heap, Count).

push(Heap, Items) -> gen_heap:push(Heap, Items).

%%%===================================================================
%%% behaviour
%%%===================================================================

init([ParentHeap, Sizes]) ->
  NewState = #state{parent = ParentHeap},
  {ok, Sizes, NewState}.

pull_up(Count, #state{parent = ParentHeap} = State) ->
  lager:debug("child_heap pull_up ~B", [Count]),
  {ok, Result} = gen_heap:pull(ParentHeap, Count),
  {ok, Result, State}.

push_up(Items, #state{parent = ParentHeap} = State) ->
  lager:debug("child_heap push_up ~B", [length(Items)]),
  ok = gen_heap:push(ParentHeap, Items),
  {ok, State}.
