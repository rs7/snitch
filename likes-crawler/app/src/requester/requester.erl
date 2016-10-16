-module(requester).

-behaviour(supervisor).

%%% api
-export([start_link/1, whereis/1]).

%%% behaviour
-export([init/1]).

-define(NAME(RequesterRef), {?MODULE, RequesterRef}).
-define(SERVER_NAME(RequesterRef), {via, identifiable, ?NAME(RequesterRef)}).

%%%===================================================================
%%% api
%%%===================================================================

start_link(RequesterRef) -> supervisor:start_link(?SERVER_NAME(RequesterRef), ?MODULE, RequesterRef).

whereis(RequesterRef) -> identifiable:whereis_name(?NAME(RequesterRef)).

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
