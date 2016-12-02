-module(test).

-behaviour(supervisor).

%%% api
-export([start_link/0]).

%%% behaviour
-export([init/1]).

%%% internal
-export([start_item/0, item_loop/1]).

%%%===================================================================
%%% api
%%%===================================================================

start_link() ->
  {ok, Pid} = supervisor:start_link({local, ?MODULE}, ?MODULE, []),
  start_children(100),
  {ok, Pid}.

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
  {ok, Pid} = supervisor:start_child(?MODULE, []),
  start_children(Count - 1).

start_item() ->
  Count = 500,
  Request = {'utils.getServerTime', #{}},
  Requests = [Request || _ <- lists:seq(1, Count)],
  {ok, spawn_link(?MODULE, item_loop, [Requests])}.

item_loop(Requests) ->
  Calls = [{vk, call, [Request]} || Request <- Requests],
  rpc:parallel_eval(Calls),
  item_loop(Requests).
