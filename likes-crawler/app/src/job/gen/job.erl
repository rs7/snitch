-module(job).

-behaviour(supervisor).

%%% api
-export([start_link/4, stop/1]).

%%% behaviour
-export([init/1]).

-include("../../util/identified_name.hrl").

-define(SERVER_NAME(JobRef), ?IDENTIFIED_NAME(?MODULE, JobRef)).

%%%===================================================================
%%% api
%%%===================================================================

start_link(Ref, Priority, Body, ControllerRef) ->
  supervisor:start_link(?SERVER_NAME(Ref), ?MODULE, {Ref, Priority, Body, ControllerRef}).

stop(Ref) -> ok.%exit(?IDENTIFIED_PID(?MODULE, Ref), shutdown).

%%%===================================================================
%%% behaviour
%%%===================================================================

init({Ref, Priority, Body, ControllerRef}) ->
  Strategy = #{strategy => one_for_all, intensity => 1, period => 5},

  Specifications = [
    #{
      id => children,
      start => {job_list, start_link, [Ref]},
      type => supervisor
    },
    #{
      id => server,
      start => {job_server, start_link, [Ref, Priority, Body, ControllerRef]},
      type => worker
    }
  ],

  {ok, {Strategy, Specifications}}.
