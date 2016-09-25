-module(job_list).

-behaviour(supervisor).

%%% api
-export([start_link/1, start_job/5, terminate_job/2]).

%%% behaviour
-export([init/1]).

-include("../../util/identified_name.hrl").

-define(SERVER_NAME(ListRef), ?IDENTIFIED_NAME(?MODULE, ListRef)).

%%%===================================================================
%%% api
%%%===================================================================

start_link(ListRef) -> supervisor:start_link(?SERVER_NAME(ListRef), ?MODULE, []).

start_job(ListRef, Ref, Priority, Body, ControllerRef) ->
  JobArgs = [Ref, Priority, Body, ControllerRef, ListRef],
  supervisor:start_child(?SERVER_NAME(ListRef), JobArgs).

terminate_job(ListRef, JobPid) -> supervisor:terminate_child(?SERVER_NAME(ListRef), JobPid).

%%%===================================================================
%%% behaviour
%%%===================================================================

init([]) ->
  Strategy = #{strategy => simple_one_for_one, intensity => 5, period => 1},

  Specifications = [
    #{
      id => job,
      restart => transient,
      start => {job, start_link, []},
      type => supervisor
    }
  ],

  {ok, {Strategy, Specifications}}.
