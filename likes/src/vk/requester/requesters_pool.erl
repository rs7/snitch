-module(requesters_pool).

-behaviour(supervisor).

%%% api
-export([start_link/0]).

%%% behaviour
-export([init/1]).

%%%===================================================================
%%% api
%%%===================================================================

start_link() ->
  {ok, Pid} = supervisor:start_link({local, ?MODULE}, ?MODULE, []),
  start_children(10),
  {ok, Pid}.

%%%===================================================================
%%% behaviour
%%%===================================================================

init([]) ->
  Strategy = #{strategy => simple_one_for_one},

  Specifications = [
    #{
      id => undefined,
      start => {requester, start_link, []}
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
