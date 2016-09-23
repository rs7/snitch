-module(requester_pool_children).

-behaviour(supervisor).

%%% api
-export([start_link/0, start_child/1, terminate_child/1]).

%%% behaviour
-export([init/1]).

-include("../util/identified_name.hrl").

-define(REQUESTER_MODULE, requester).

%%%===================================================================
%%% api
%%%===================================================================

start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(RequesterRef) -> supervisor:start_child(?MODULE, [RequesterRef]).

terminate_child(RequesterRef) -> supervisor:terminate_child(?MODULE, ?IDENTIFIED_PID(?REQUESTER_MODULE, RequesterRef)).

%%%===================================================================
%%% behaviour
%%%===================================================================

init([]) ->
  Strategy = #{strategy => simple_one_for_one, intensity => 5, period => 1},

  Specifications = [
    #{
      id => requester,
      start => {?REQUESTER_MODULE, start_link, []},
      type => worker
    }
  ],

  {ok, {Strategy, Specifications}}.
