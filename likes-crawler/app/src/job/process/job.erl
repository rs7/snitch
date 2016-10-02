-module(job).

-behaviour(supervisor).

%%% api
-export([start_link/5, stop/2]).

%%% behaviour
-export([init/1]).

-include("../../util/identified_name.hrl").

-define(SERVER_NAME(JobRef), ?IDENTIFIED_NAME(?MODULE, JobRef)).

%%%===================================================================
%%% api
%%%===================================================================

start_link(Ref, Priority, Body, ControllerRef, ListRef) ->
  supervisor:start_link(?SERVER_NAME(Ref), ?MODULE, {Ref, Priority, Body, ControllerRef, ListRef}).

stop(Ref, ListRef) ->
  Pid = ?IDENTIFIED_PID(?MODULE, Ref),
  job_list:terminate_job(ListRef, Pid).

%%%===================================================================
%%% behaviour
%%%===================================================================

init({Ref, Priority, Body, ControllerRef, ListRef}) ->
  Strategy = #{strategy => one_for_all, intensity => 1, period => 5},

  Specifications = [
    #{
      id => children,
      start => {job_list, start_link, [Ref]},
      type => supervisor
    },
    #{
      id => server,
      start => {job_server, start_link, [Ref, Priority, Body, ControllerRef, ListRef]},
      type => worker
    }
  ],

  {ok, {Strategy, Specifications}}.
