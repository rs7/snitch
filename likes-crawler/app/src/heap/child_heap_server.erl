-module(child_heap_server).

-behaviour(gen_heap).

%%% api
-export([start_link/0, pull/1, push/1]).

%%% behaviour
-export([init/1, pull/2, push/2]).

-record(state, {n}).

%%%===================================================================
%%% api
%%%===================================================================

start_link() -> gen_heap:start_link({local, ?MODULE}, ?MODULE, []).

pull(Count) -> gen_heap:pull(?MODULE, Count).

push(Items) -> gen_heap:push(?MODULE, Items).

%%%===================================================================
%%% behaviour
%%%===================================================================

init([]) -> {ok, 5, 17, 10, #state{n = 0}}.

pull(Count, #state{n = N} = State) ->
  Result = [X || X <- lists:seq(N + 1, N + Count)],
  io:format("pull ~p ~n", [Result]),
  NewState = State#state{n = N + Count},
  {ok, Result, NewState}.

push(Items, State) ->
  io:format("push ~p ~n", [Items]),
  {ok, State}.
