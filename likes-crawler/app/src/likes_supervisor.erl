-module(likes_supervisor).

-behaviour(supervisor).

%%% api
-export([start_link/0]).

%%% behaviour
-export([init/1]).

%%%===================================================================
%%% api
%%%===================================================================

start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%===================================================================
%%% behaviour
%%%===================================================================

init([]) ->
  Strategy = #{strategy => one_for_all, intensity => 0, period => 1},

  TestJobData = {request_job, {filter_users, [1,2,3,4,5,6]}},

  Specifications = [
    #{
      id => call_queue,
      start => {call_queue, start_link, []},
      type => worker
    },
    #{
      id => requester,
      start => {requester, start_link, [make_ref()]},
      type => worker
    },
    #{
      id => controller,
      start => {mock_controller, start_link, []},
      type => worker
    },
    #{
      id => job,
      start => {job, start_link, [mock_controller, make_ref(), TestJobData]},
      type => supervisor
    }
  ],

  {ok, {Strategy, Specifications}}.
