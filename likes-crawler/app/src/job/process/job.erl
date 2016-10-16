-module(job).

-behaviour(supervisor).

%%% api
-export([start_link/5, stop/2, whereis/1]).

%%% behaviour
-export([init/1]).

-define(NAME(JobRef), {?MODULE, JobRef}).
-define(SERVER_NAME(JobRef), {via, identifiable, ?NAME(JobRef)}).

%%%===================================================================
%%% api
%%%===================================================================

start_link(Ref, Priority, Body, ControllerRef, ListRef) ->
  supervisor:start_link(?SERVER_NAME(Ref), ?MODULE, {Ref, Priority, Body, ControllerRef, ListRef}).

stop(Ref, ListRef) -> job_list:terminate_job(ListRef, Ref).

whereis(Ref) -> identifiable:whereis_name(?NAME(Ref)).

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
