-module(job_list).

-behaviour(supervisor).

%%% api
-export([start_link/1, start_job/2]).

%%% behaviour
-export([init/1]).

-include("../../util/identified_name.hrl").

-define(SERVER_NAME(ListRef), ?IDENTIFIED_NAME(?MODULE, ListRef)).

%%%===================================================================
%%% api
%%%===================================================================

start_link(ListRef) -> supervisor:start_link(?SERVER_NAME(ListRef), ?MODULE, []).

start_job(ListRef, JobArgs) -> supervisor:start_child(?SERVER_NAME(ListRef), JobArgs).

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
