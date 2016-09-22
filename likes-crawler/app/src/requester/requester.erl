-module(requester).

-behaviour(supervisor).

%%% api
-export([start_link/1]).

%%% behaviour
-export([init/1]).

-include("../util/identified_name.hrl").

%%%===================================================================
%%% api
%%%===================================================================

start_link(RequesterRef) ->
  supervisor:start_link(?IDENTIFIED_NAME(?MODULE, RequesterRef), ?MODULE, RequesterRef).

%%%===================================================================
%%% behaviour
%%%===================================================================

init(RequesterRef) ->
  Strategy = #{strategy => one_for_all, intensity => 1, period => 5},

  Specifications = [
    #{
      id => controller,
      start => {requester_controller, start_link, [RequesterRef]},
      type => worker
    },
    #{
      id => connection,
      start => {requester_connection, start_link, [RequesterRef]},
      type => worker
    }
  ],

  {ok, {Strategy, Specifications}}.
