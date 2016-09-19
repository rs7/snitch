-module(heap).

-behaviour(gen_heap).

%%% api
-export([start_link/2, start_link/3]).

%%% behaviour
-export([init/1, pull_from_up_level/2, push_to_up_level/2]).

-record(state, {parent_heap_ref}).

%%%===================================================================
%%% api
%%%===================================================================

start_link(ParentHeapRef, Sizes) -> gen_heap:start_link(?MODULE, [ParentHeapRef, Sizes]).

start_link(HeapName, ParentHeapRef, Sizes) -> gen_heap:start_link(HeapName, ?MODULE, [ParentHeapRef, Sizes]).

%%%===================================================================
%%% behaviour
%%%===================================================================

init([ParentHeapRef, Sizes]) ->
  NewState = #state{parent_heap_ref = ParentHeapRef},
  {ok, Sizes, NewState}.

pull_from_up_level(Count, #state{parent_heap_ref = ParentHeapRef} = State) ->
  {ok, Result} = gen_heap:pull(ParentHeapRef, Count),
  {ok, Result, State}.

push_to_up_level(Items, #state{parent_heap_ref = ParentHeapRef} = State) ->
  ok = gen_heap:push(ParentHeapRef, Items),
  {ok, State}.
