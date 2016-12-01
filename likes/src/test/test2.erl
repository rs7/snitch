-module(test2).

-behaviour(supervisor).

%%% api
-export([start_link/0]).

%%% behaviour
-export([init/1]).

%%% internal
-export([start_item/0, item_loop/1, proc/2]).

%%%===================================================================
%%% api
%%%===================================================================

start_link() ->
  Result = supervisor:start_link({local, ?MODULE}, ?MODULE, []),
  start_children(1),
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
  Count = 100,
  Requests = [{'utils.getServerTime', #{}} || _ <- lists:seq(1, Count)],

  {ok, spawn_link(?MODULE, item_loop, [Requests])}.

item_loop(Requests) ->

  {Time, _Value} = timer:tc(?MODULE, proc, [Requests, 10]),

  lager:info("~.3f req/sec", [1000000000 / Time]),

  item_loop(Requests).

proc(Requests, 0) -> ok;

proc(Requests, Count) ->
  Send = request:create(Requests),
  Recv = socket:process(Send),
  Responses = response:parse(Recv, length(Requests)),
  proc(Requests, Count - 1).
