-module(requester_pool).

-behaviour(supervisor).

%%% api
-export([start_link/1]).

%%% behaviour
-export([init/1]).

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
  Strategy = #{strategy => simple_one_for_one, intensity => 5, period => 1},

  Specifications = [
    #{
      id => requester,
      start => {requester, start_link, []},
      type => worker
    }
  ],

  {ok, {Strategy, Specifications}}.

%%%===================================================================
%%% internal
%%%===================================================================

start_children(0) -> ok;

start_children(Count) ->
  start_child(Count),
  start_children(Count - 1).

start_child(RequesterId) -> supervisor:start_child(?MODULE, [RequesterId]).
