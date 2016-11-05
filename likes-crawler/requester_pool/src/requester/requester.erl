-module(requester).

-behaviour(supervisor).

%%% api
-export([start_link/1, whereis/1]).

%%% behaviour
-export([init/1]).

-define(SERVER_NAME(RequesterRef), {via, identifiable, {?MODULE, RequesterRef}}).

%%%===================================================================
%%% api
%%%===================================================================

start_link(RequesterRef) -> supervisor:start_link(?SERVER_NAME(RequesterRef), ?MODULE, RequesterRef).

whereis(RequesterRef) -> util:whereis_name(?SERVER_NAME(RequesterRef)).

%%%===================================================================
%%% behaviour
%%%===================================================================

init(RequesterRef) ->
  Strategy = #{strategy => one_for_all, intensity => 1, period => 5},

  Specifications = [
    #{
      id => queue,
      start => {requester_queue, start_link, [RequesterRef]},
      type => worker
    },
    #{
      id => connection,
      start => {requester_connection, start_link, [RequesterRef]},
      type => worker
    }
  ],

  {ok, {Strategy, Specifications}}.
