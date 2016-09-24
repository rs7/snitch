-module(wor).

-behaviour(supervisor).

%%% api
-export([start_link/0, stop/1]).

%%% behaviour
-export([init/1]).

%%%===================================================================
%%% api
%%%===================================================================

start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

stop(Reason) ->
  Pid = whereis(?MODULE),
  io:format("stop ~p ~p~n", [Reason, Pid]),
  exit(Pid, Reason).

%%%===================================================================
%%% behaviour
%%%===================================================================

init([]) ->
  Strategy = #{strategy => one_for_all, intensity => 1, period => 5},

  Specifications = [
    #{
      id => child,
      start => {test_controller, start_link, []},
      type => worker
    }
  ],

  {ok, {Strategy, Specifications}}.
