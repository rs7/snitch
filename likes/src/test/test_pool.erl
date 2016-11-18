-module(test_pool).

-behaviour(supervisor).

%%% api
-export([start_link/1]).

%%% behaviour
-export([init/1]).

%%% internal
-export([start_item/0, item_loop/1]).

%%%===================================================================
%%% api
%%%===================================================================

start_link(Size) ->
  Result = supervisor:start_link({local, ?MODULE}, ?MODULE, []),
  start_children(Size),
  Result.

%%%===================================================================
%%% behaviour
%%%===================================================================

init([]) ->
  Strategy = #{strategy => simple_one_for_one},

  Specifications = [
    #{
      id => undefined,
      start => {?MODULE, start_item, []}
    }
  ],

  {ok, {Strategy, Specifications}}.

%%%===================================================================
%%% internal
%%%===================================================================

start_children(0) -> ok;

start_children(Count) ->
  supervisor:start_child(?MODULE, []),
  start_children(Count - 1).

start_item() ->
  Count = 700,
  Requests = [{'utils.getServerTime', #{}} || _ <- lists:seq(1, Count)],
  {ok, spawn_link(?MODULE, item_loop, [Requests])}.

item_loop(Requests) ->

  rpc:parallel_eval(
    [{vk, call, [Request]} || Request <- Requests]
  ),

  item_loop(Requests).
