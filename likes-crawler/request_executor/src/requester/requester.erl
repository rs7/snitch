-module(requester).

-behaviour(supervisor).

%%% api
-export([start_link/1]).

%%% behaviour
-export([init/1]).

-define(SERVER_NAME(RequesterId), {via, identifiable, {?MODULE, RequesterId}}).

%%%===================================================================
%%% api
%%%===================================================================

start_link(RequesterId) -> supervisor:start_link(?SERVER_NAME(RequesterId), ?MODULE, RequesterId).

%%%===================================================================
%%% behaviour
%%%===================================================================

init(RequesterId) ->
  Strategy = #{strategy => one_for_all, intensity => 1, period => 5},

  Specifications = [
    #{
      id => queue,
      start => {requester_queue, start_link, [RequesterId]},
      type => worker
    },
    #{
      id => connection,
      start => {requester_connection, start_link, [RequesterId]},
      type => worker
    },
    #{
      id => stream_controller,
      start => {requester_stream_controller, start_link, [RequesterId]},
      type => worker
    }
  ],

  {ok, {Strategy, Specifications}}.