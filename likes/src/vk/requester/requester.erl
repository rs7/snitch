-module(requester).

-behaviour(supervisor).

%%% api
-export([start_link/0]).

%%% behaviour
-export([init/1]).

%%%===================================================================
%%% api
%%%===================================================================

start_link() -> supervisor:start_link(?MODULE, []).

%%%===================================================================
%%% behaviour
%%%===================================================================

init([]) ->
  Strategy = #{strategy => one_for_all},

  Id = make_ref(),

  Specifications = [
    #{
      id => queue,
      start => {requester_queue, start_link, [Id]}
    },
    #{
      id => proc,
      start => {requester_proc, start_link, [Id]}
    }
  ],

  {ok, {Strategy, Specifications}}.
